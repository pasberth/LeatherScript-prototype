{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LeatherScript.LeatherShield where

import           Control.Lens
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans
import           Data.Composition((.:))
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Language.LeatherScript.AST     as AST

type Identifier = Text.Text
type TypeEnv = HashMap.HashMap Identifier Type
type TypeSynonyms = HashMap.HashMap Identifier Type

data Type
  = SimpleTy Text.Text
  | ArrowTy Type Type
  | PairTy Type Type
  | UPairTy Type Type
  | EitherTy Type Type
  | VariantTy Text.Text Type
  | RefTy Type
  | UnitTy
  deriving (Show, Eq)

data TypeError
  = TypeError Type Type
  | NotInScope Identifier
  deriving (Show)

data LeatherShieldState
  = LeatherShieldState
    {
      _typeEnv :: TypeEnv
    , _typeSynonyms :: TypeSynonyms
    }

makeLenses ''LeatherShieldState

newtype LeatherShieldT m a
  = LeatherShieldT
    {
      unLeatherShieldT :: StateT LeatherShieldState (EitherT TypeError m) a
    }
  deriving (Functor, Applicative, Monad, MonadState LeatherShieldState)

emptyLeatherShield :: LeatherShieldState
emptyLeatherShield = LeatherShieldState { _typeEnv = HashMap.empty, _typeSynonyms = HashMap.empty }

runLeatherShieldT :: Monad m => LeatherShieldT m a -> LeatherShieldState -> m (Either TypeError a)
runLeatherShieldT = runEitherT .: (evalStateT . unLeatherShieldT)

forceMkName :: AST.AST  -> Identifier
forceMkName (AST.Identifier x _) = x
forceMkName x = error ("bad identifier: " ++ show x)

forceMkType :: AST.AST -> TypeSynonyms -> Type
forceMkType (AST.SimpleType s) e = SimpleTy $ forceMkName s
forceMkType (AST.Identifier s _) e = case HashMap.lookup s e of
                                       Just ty -> ty
                                       _ -> error ("not found;" ++ show s)

typeError :: Monad m => TypeError -> LeatherShieldT m a
typeError = LeatherShieldT . lift . left

leatherShield :: Monad m => AST.AST -> LeatherShieldT m Type
leatherShield (AST.TypeSynonym name ty) = do
  synonyms <- use typeSynonyms
  let ident = forceMkName name
  let typ = forceMkType ty synonyms
  typeSynonyms %= HashMap.insert ident typ
  return UnitTy
leatherShield (AST.Sequence x y) = do
  x <- leatherShield x
  case x of
    UnitTy -> leatherShield y
    got -> typeError $ TypeError UnitTy got
leatherShield (AST.Assign x y) = do
  let ident = forceMkName x
  yTy <- leatherShield y
  typeEnv %= HashMap.insert ident yTy
  return UnitTy
leatherShield (AST.Abstraction x y) = do
  synonyms <- use typeSynonyms
  let ty = paramTy x synonyms
  let tys = paramTys x synonyms
  typeEnv %= HashMap.union tys
  bodyTy <- leatherShield y
  return $ ArrowTy ty bodyTy
leatherShield (AST.Ascribe name ty) = do
  synonyms <- use typeSynonyms
  let ident = forceMkName name
  let typ = forceMkType ty synonyms
  typeEnv %= HashMap.insert ident typ
  return UnitTy
leatherShield (AST.Identifier name _) = do
  x <- uses typeEnv (HashMap.lookup name)
  case x of
    Just ty -> return ty
    Nothing -> typeError $ NotInScope name
leatherShield (AST.Match x ys) = do
  ty <- leatherShield x
  tys <- mapM (\(y,z) -> leatherShield y) ys
  if all (flip eitherInclude ty) tys
    then do
         zTys <- mapM (\(_,z) -> leatherShield z) ys
         if all (== head zTys) zTys
            then return $ head zTys
            else typeError $ TypeError (head zTys) (zTys !! 1)

    else do
      typeError $ TypeError ty $ Maybe.fromJust (List.find (not . flip eitherInclude ty) tys)

leatherShield (AST.Variant x y) = do
  let ident = forceMkName x
  ty <- leatherShield y
  return $ VariantTy ident ty

eitherInclude :: Type -> Type -> Bool
eitherInclude ty1 (EitherTy ty2 ty3) = eitherInclude ty1 ty2 || eitherInclude ty1 ty3
eitherInclude ty1 ty2 = ty1 == ty2

paramTy (AST.Ascribe _ t) e = forceMkType t e
paramTy (AST.OrderedPair x y) e = PairTy (paramTy x e) (paramTy y e)

paramTys (AST.Ascribe n t) e = HashMap.singleton (forceMkName n) (forceMkType t e)
paramTys (AST.OrderedPair x y) e = HashMap.union (paramTys x e) (paramTys y e)
