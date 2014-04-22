{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Language.LeatherScript.Parser where

import           Control.Lens                   hiding (Level, parts)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Loop
import           Data.Composition((.:))
import qualified Data.Maybe                     as Maybe
import qualified Data.List                      as List
import qualified Data.Text                      as Text
import qualified Data.Vector                    as Vector
import qualified Data.HashSet                   as HashSet
import qualified Data.HashMap.Strict            as HashMap
import qualified Language.LeatherScript.Types()

type Variable = Text.Text
type Keyword = Text.Text

data NotationPart
  = Variable Variable
  | Keyword Keyword
  deriving (Show)

type NotationParts = Vector.Vector NotationPart
type Replacement = SyntaxTree

data Associativity
  = NoAssoc
  | LeftAssoc
  | RightAssoc
  deriving (Show)

type Level = Int

data Pattern
  = Prefix Keyword NotationParts Variable
  | Postfix Variable NotationParts Keyword
  | Outfix Keyword NotationParts Keyword
  | Infix Variable NotationParts Variable
  | Alias Keyword
  deriving (Show)

data Notation
  = Notation
    { _pattern :: Pattern
    , _replacement :: Replacement
    , _associativity :: Associativity
    , _level :: Level
    }
  deriving (Show)

type Keywords = HashSet.HashSet Keyword
type Notations = HashMap.HashMap Keyword Notation

type NotationStackValue = (Notation, Vector.Vector SyntaxTree, NotationParts)
type NotationStack = Vector.Vector NotationStackValue

data SyntaxTree
  = Token Text.Text Int
  | Preference (Vector.Vector SyntaxTree)

instance Eq SyntaxTree where
  (Token tk1 _) == (Token tk2 _) = tk1 == tk2
  (Preference v1) == (Preference v2)
    | Vector.length v1 == Vector.length v2 = Vector.all (uncurry (==)) (Vector.zip v1 v2)
    | otherwise = False
  _ == _ = False

instance Show SyntaxTree where
  show (Token txt _) = Text.unpack txt
  show (Preference v) = "(" ++ (join $ List.intersperse " " $ Vector.toList $ Vector.map show v) ++ ")"

data ParseError
  = Expecting Text.Text
  | Unexpected Text.Text
  | CantAssoc Text.Text
  | NotEnough
  deriving (Show, Eq)

type ParserStack = Vector.Vector SyntaxTree

data ParserState
  = ParserState
    {_keywords :: Keywords
    , _notations :: Notations
    , _notationStack :: NotationStack
    , _parserStack :: ParserStack
    , _tokens :: Vector.Vector Text.Text
    , _tokenno :: !Int
    }

newtype ParserT m a
  = ParserT
    {
      unParserT :: EitherT ParseError (StateT ParserState m) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState ParserState
    , MonadIO
    )

type Parser = ParserT Identity

makeLenses ''Notation
makeLenses ''ParserState

runParserT :: Monad m => ParserT m a -> ParserState -> m (Either ParseError a)
runParserT = evalStateT . runEitherT . unParserT

runParser :: Parser a -> ParserState -> Either ParseError a
runParser = runIdentity .: (evalStateT . runEitherT . unParserT)

parseError :: Monad m => ParseError -> ParserT m a
parseError err = ParserT $ left err

countVariableInNotationPart :: NotationPart -> Int
countVariableInNotationPart (Variable _) = 1
countVariableInNotationPart (Keyword _) = 0

countVariableInNotationParts :: NotationParts -> Int
countVariableInNotationParts parts = Vector.sum $ Vector.map countVariableInNotationPart parts

countVariableInPattern :: Pattern -> Int
countVariableInPattern (Prefix _ parts _) = countVariableInNotationParts parts + 1
countVariableInPattern (Postfix _ parts _) = countVariableInNotationParts parts + 1
countVariableInPattern (Outfix _ parts _) = countVariableInNotationParts parts
countVariableInPattern (Infix _ parts _) = countVariableInNotationParts parts + 2
countVariableInPattern (Alias _) = 0

countVariableInReplacement :: Replacement -> Int
countVariableInReplacement (Token (Text.uncons -> Just ('$', _)) _) = 1
countVariableInReplacement (Token _ _) = 0
countVariableInReplacement (Preference v) = Vector.sum (Vector.map countVariableInReplacement v)

variablesInNotationPart :: NotationPart -> Vector.Vector Variable
variablesInNotationPart (Variable v) = Vector.singleton v
variablesInNotationPart (Keyword _) = Vector.empty

variablesInNotationParts :: NotationParts -> Vector.Vector Variable
variablesInNotationParts parts = Vector.concatMap variablesInNotationPart parts

variablesInPattern :: Pattern -> Vector.Vector Variable
variablesInPattern (Prefix _ parts v) = Vector.snoc (variablesInNotationParts parts) v
variablesInPattern (Postfix v parts _) = Vector.cons v (variablesInNotationParts parts)
variablesInPattern (Outfix _ parts _) = variablesInNotationParts parts
variablesInPattern (Infix v1 parts v2) = Vector.cons v1 (Vector.snoc (variablesInNotationParts parts) v2)
variablesInPattern (Alias _) = []

keywordsInNotationPart :: NotationPart -> Vector.Vector Keyword
keywordsInNotationPart (Variable _) = Vector.empty
keywordsInNotationPart (Keyword k) = Vector.singleton k

keywordsInNotationParts :: NotationParts -> Vector.Vector Keyword
keywordsInNotationParts parts = Vector.concatMap keywordsInNotationPart parts

keywordsInPattern :: Pattern -> Vector.Vector Keyword
keywordsInPattern (Prefix k parts _) = Vector.cons k (keywordsInNotationParts parts)
keywordsInPattern (Postfix _ parts k) = Vector.snoc (keywordsInNotationParts parts) k
keywordsInPattern (Outfix k1 parts k2) = Vector.cons k1 (Vector.snoc (keywordsInNotationParts parts) k2)
keywordsInPattern (Infix _ parts _) = keywordsInNotationParts parts
keywordsInPattern (Alias k) = [k]

patternFromVector :: Vector.Vector Text.Text -> Pattern
patternFromVector v = do
    let w = Vector.map convert v
    case Vector.length w of
      0 -> error "oh!"
      1 -> case Vector.head w of
            Keyword k -> Alias k
            _ -> error "you cannot define a variable pattern"
      _ -> do
        let u = Vector.init (Vector.tail w)
        case (Vector.head w, Vector.last w) of
          (Keyword x, Variable y) -> Prefix x u y
          (Variable x, Keyword y) -> Postfix x u y
          (Keyword x, Keyword y) -> Outfix x u y
          (Variable x, Variable y) -> Infix x u y
  where
    convert variable@(Text.uncons -> Just ('$', _)) = Variable variable
    convert keyword = Keyword keyword

patternAsVector :: Pattern -> NotationParts
patternAsVector (Prefix x y z) = Vector.cons (Keyword x) (Vector.snoc y (Variable z))
patternAsVector (Postfix x y z) = Vector.cons (Variable x) (Vector.snoc y (Keyword z))
patternAsVector (Outfix x y z) = Vector.cons (Keyword x) (Vector.snoc y (Keyword z))
patternAsVector (Infix x y z) = Vector.cons (Variable x) (Vector.snoc y (Variable z))
patternAsVector (Alias x) = Vector.singleton (Keyword x)

mkEnvironment :: Pattern -> Vector.Vector SyntaxTree -> HashMap.HashMap Variable SyntaxTree
mkEnvironment pattern arguments = do
  let variables = variablesInPattern pattern
  HashMap.fromList $ Vector.toList $ Vector.zip variables arguments

subst :: Replacement -> HashMap.HashMap Variable SyntaxTree -> SyntaxTree
subst (Token v@(Text.uncons -> Just ('$', _)) _) e = Maybe.fromJust $ HashMap.lookup v e
subst st@(Token _ _) _ = st
subst (Preference v) e = Preference (Vector.map (\st -> subst st e) v)

parse :: Monad m => Vector.Vector Text.Text -> ParserT m SyntaxTree
parse _tokens = do
  tokens .= _tokens
  let rec = do
        parse1
        ParserState{_tokens} <- get
        case _tokens of
          [] -> do
            ParserState{_notationStack} <- get
            Vector.forM_ _notationStack $ \_ -> takeOperand >> reduce1
            uses notations (HashMap.lookup "") >>= \case
              Nothing -> uses parserStack Vector.reverse <&> \case
                [x] -> x
                xs -> Preference xs
              Just notation -> do
                use parserStack <&> Vector.foldl1 (\x f -> do
                  let e = mkEnvironment (notation ^. pattern) [f, x]
                  subst (notation ^. replacement) e)
          _ -> rec
  rec

takeOperand :: Monad m => ParserT m ()
takeOperand = do
  operands <- use (notationStack . element 0 . _2)
  use parserStack >>= \case
    [] -> parseError NotEnough
    (Vector.head -> operand) -> do
      let newOperands = Vector.snoc operands operand
      notationStack . element 0 . _2 .= newOperands
      notationStack . element 0 . _3 %= Vector.tail
      parserStack %= Vector.tail

reduce :: Monad m => ParserT m ()
reduce = do
  (notation, operands, unconsumedParts) <- uses notationStack Vector.head
  case unconsumedParts of
    [] -> do
      use parserStack >>= \case
        [] -> reduce1
        _ -> do
          uses notations (HashMap.lookup "") >>= \case
            Nothing -> do
              reduce1
            Just notation -> do
              reduce1
              st <- uses parserStack Vector.head
              parserStack %= Vector.tail
              reduceGroup notation
              left <- uses parserStack Vector.head
              parserStack %= Vector.tail
              let e = mkEnvironment (notation ^. pattern) [left, st]
              parserStack %= Vector.cons (subst (notation ^. replacement) e)
    _ -> reduce1

reduce1 :: Monad m => ParserT m ()
reduce1 = do
  (notation, operands, unconsumedParts) <- uses notationStack Vector.head
  case unconsumedParts of
    [] -> do
      let e = mkEnvironment (notation ^. pattern) operands
      let st = subst (notation ^. replacement) e
      notationStack %= Vector.tail
      parserStack %= Vector.cons st

    (Vector.head -> Keyword expectingKeyword) -> do
      parseError $ Expecting expectingKeyword
    (Vector.head -> Variable expectingKeyword) -> do
      parseError $ NotEnough

reduceGroup :: Monad m => Notation -> ParserT m ()
reduceGroup notation = do
  ParserState{_notationStack} <- get
  foreach (Vector.toList _notationStack) $ \(left, arguments, unconsumedKeywords) -> do
    case left ^. pattern of
      Alias _ ->
        lift $ reduce1
      Outfix _ _ _ ->
        exit
      _ -> do
        if
          | (countVariableInPattern (left ^. pattern) - Vector.length arguments) == 1 -> do
            if
              | (left ^. level) < (notation ^. level) -> do
                exit
              | (left ^. level) > (notation ^. level) -> do
                lift $ do
                  takeOperand
                  reduce1
              | otherwise -> do
                case (left ^. associativity, notation ^. associativity) of
                  (LeftAssoc, LeftAssoc) ->
                    lift $ do
                      takeOperand
                      reduce1
                  (RightAssoc, RightAssoc) ->
                    exit
                  _ ->
                    if Vector.length (keywordsInPattern (notation ^. pattern)) == 0
                      then lift $ parseError $ CantAssoc ""
                      else lift $ parseError $ CantAssoc $ Vector.head $ keywordsInPattern $ notation ^. pattern
          | otherwise -> do
            exit

parse1 :: Monad m => ParserT m ()
parse1 = do
  ParserState{_tokens, _keywords} <- get
  if
    | Vector.length _tokens == 0 -> do
      return ()
    | HashSet.member (Vector.head _tokens) _keywords -> do
      let kw = Vector.head _tokens
      tokenno += 1
      ParserState{_notations} <- get
      case HashMap.lookup kw _notations of
        Just notation -> do
          case notation of
            Notation (Prefix _ _ _) _ _ _ -> do
              notationStack %= Vector.cons (notation, [], Vector.drop 1 (patternAsVector (notation ^. pattern)))
              tokens %= Vector.tail
            Notation (Postfix _ [] _) _ _ _ -> do
              reduceGroup notation
              left <- uses parserStack Vector.head
              notationStack %= Vector.cons (notation, [left], [])
              parserStack %= Vector.tail
              tokens %= Vector.tail
              (notation, operands, unconsumedKeywords) <- uses notationStack Vector.head
              let e = mkEnvironment (notation ^. pattern) operands
              let st = subst (notation ^. replacement) e
              notationStack %= Vector.tail
              parserStack %= Vector.cons st
            Notation (Postfix _ _ _) _ _ _ -> do
              reduceGroup notation
              left <- uses parserStack Vector.head
              notationStack %= Vector.cons (notation, [left], Vector.drop 2 (patternAsVector (notation ^. pattern)))
              parserStack %= Vector.tail
              tokens %= Vector.tail
            Notation (Outfix open _ close) _ _ _ -> do
              notationStack %= Vector.cons (notation, [], Vector.drop 1 (patternAsVector (notation ^. pattern)))
              tokens %= Vector.tail

              -- for instance, "| x |"
              when (open == close) $ do
                notations %= HashMap.delete kw
            Notation (Infix _ _ _) _ _ _ -> do
              reduceGroup notation
              left <- uses parserStack Vector.head
              notationStack %= Vector.cons (notation, [left], Vector.drop 2 (patternAsVector (notation ^. pattern)))
              parserStack %= Vector.tail
              tokens %= Vector.tail
            Notation (Alias _) _ _ _ -> do
              notationStack %= Vector.cons (notation, [], [])
              tokens %= Vector.tail
              reduce1
        Nothing -> do
          ParserState{_notationStack} <- get
          foreach (Vector.toList _notationStack) $ \(notation, arguments, unconsumedParts) -> do
            case unconsumedParts of
              [] ->
                lift $ reduce1
              ((Vector.!? 1) -> Just (Keyword expectingKeyword)) -> do
                if kw == expectingKeyword
                  then do
                    exit
                  else do
                    lift $ parseError $ Unexpected kw
              (Vector.head -> Variable _) -> do
                lift $ do
                  takeOperand
                  reduce1
          (notation, arguments, unconsumedParts) <- uses notationStack Vector.head
          if countVariableInPattern (notation ^. pattern) - Vector.length arguments == 1
            then do
              takeOperand
              notationStack . element 0 . _3 %= Vector.tail -- drop kw
              reduce1
            else do
              takeOperand
              notationStack . element 0 . _3 %= Vector.tail -- drop kw
          case notation ^. pattern of
            Outfix open _ close
              | open == close -> do
                notations %= HashMap.insert open notation
              | otherwise -> do
                return ()
            _ -> do
              return ()
          tokens %= Vector.tail
    | otherwise -> do
      tk <- uses tokens Vector.head
      i <- use tokenno
      tokenno += 1
      let st = Token tk i

      uses tokens (Vector.!? 1) >>= \case
        Nothing -> do
          parserStack %= Vector.cons st
          tokens %= Vector.tail
        Just tk
          | HashSet.member tk _keywords -> do
            notationMaybe <- HashMap.lookup tk <$> use notations
            case notationMaybe of
              Nothing -> do
                parserStack %= Vector.cons st
                tokens %= Vector.tail
              Just notation -> do
                case notation ^. pattern of
                  Postfix _ _ _ -> do
                    parserStack %= Vector.cons st
                    tokens %= Vector.tail
                  Infix _ _ _ -> do
                    parserStack %= Vector.cons st
                    tokens %= Vector.tail
                  _ -> do
                    uses notations (HashMap.lookup "") >>= \case
                      Nothing -> do
                        parserStack %= Vector.cons st
                        tokens %= Vector.tail
                      Just notation -> do
                        parserStack %= Vector.cons st
                        reduceGroup notation
                        left <- uses parserStack Vector.head
                        parserStack %= Vector.tail
                        notationStack %= Vector.cons (notation, [left], Vector.drop 1 (patternAsVector (notation ^. pattern)))
                        tokens %= Vector.tail
          | otherwise -> do
            uses notations (HashMap.lookup "") >>= \case
              Nothing -> do
                parserStack %= Vector.cons st
                tokens %= Vector.tail
              Just notation -> do
                parserStack %= Vector.cons st
                reduceGroup notation
                left <- uses parserStack Vector.head
                parserStack %= Vector.tail
                notationStack %= Vector.cons (notation, [left], Vector.drop 1 (patternAsVector (notation ^. pattern)))
                tokens %= Vector.tail

emptyParserState :: ParserState
emptyParserState
  = ParserState
    { _keywords = HashSet.empty
    , _notations = HashMap.empty
    , _notationStack = []
    , _parserStack = []
    , _tokens = []
    , _tokenno = 0
    }
