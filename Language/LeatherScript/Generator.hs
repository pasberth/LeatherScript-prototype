{-# LANGUAGE OverloadedStrings          #-}

module Language.LeatherScript.Generator where

import qualified Data.Text                      as Text
import qualified Data.Aeson                     as Aeson
import qualified Language.LeatherScript.AST     as AST

data JavaScriptAST
  = Identifier Text.Text
  | Function [JavaScriptAST] JavaScriptAST
  | Call JavaScriptAST [JavaScriptAST]
  | Conditional JavaScriptAST JavaScriptAST JavaScriptAST
  | Assign JavaScriptAST JavaScriptAST
  | Sequence JavaScriptAST JavaScriptAST
  | Not JavaScriptAST
  | And JavaScriptAST JavaScriptAST
  | Or JavaScriptAST JavaScriptAST
  | Eq JavaScriptAST JavaScriptAST
  | Add JavaScriptAST JavaScriptAST
  | Sub JavaScriptAST JavaScriptAST
  | Mul JavaScriptAST JavaScriptAST
  | Div JavaScriptAST JavaScriptAST
  | Member JavaScriptAST JavaScriptAST
  | NTuple [JavaScriptAST]
  | Object [(JavaScriptAST, JavaScriptAST)]
  | StrLit Text.Text
  | IntLit Int

fromAST :: AST.AST -> JavaScriptAST
fromAST (AST.Identifier ident _) = Identifier ident
fromAST (AST.Application x y) = Call (fromAST x) (mkArgs y) where
  mkArgs (AST.OrderedPair x y) = fromAST x : mkArgs y
  mkArgs AST.Unit = []
  mkArgs x = [fromAST x]
fromAST (AST.Abstraction x y) = Function (mkParams x) (fromAST y) where
  mkParams (AST.OrderedPair x y) = fromAST x : mkParams y
  mkParams AST.Unit = []
  mkParams x = [fromAST x]
fromAST (AST.Conditional x y z) = Conditional (fromAST x) (fromAST y) (fromAST z)
fromAST (AST.Assign x y) = Assign (fromAST x) (fromAST y)
fromAST (AST.Sequence x y) = case x of
                               AST.SimpleType _ -> fromAST y
                               AST.TypeSynonym _ _ -> fromAST y
                               AST.Ascribe (AST.Identifier _ _) _ -> fromAST y
                               _ -> Sequence (fromAST x) (fromAST y)
fromAST (AST.Member x y) = Member (fromAST x) (fromAST y)
fromAST (AST.Variant x y) = Object [(fromAST x, fromAST y)]
fromAST (AST.Unit) = NTuple []
fromAST (AST.OrderedPair x y) = case (fromAST x, fromAST y) of
  (x', NTuple ys) -> NTuple (x':ys)
  (x', y') -> NTuple [x',y']
fromAST (AST.UnorderedPair x y) = case (fromAST x, fromAST y) of
  (Object x', Object y') -> Object (x'++y')
fromAST (AST.StrLit s) = StrLit s
fromAST (AST.Match x yz) = do
  let tmp = (Assign (Identifier "it") (fromAST x))
  let matching = foldr (\(y,z) a -> Conditional (mkTest y (Identifier "it")) (fromAST z) a ) (Identifier "undefined") yz
  Sequence tmp matching
fromAST (AST.Not x) = Not (fromAST x)
fromAST (AST.And x y) = And (fromAST x) (fromAST y)
fromAST (AST.Or x y) = Or (fromAST x) (fromAST y)
fromAST (AST.Eq x y) = Eq (fromAST x) (fromAST y)
fromAST (AST.Add x y) = Add (fromAST x) (fromAST y)
fromAST (AST.Sub x y) = Sub (fromAST x) (fromAST y)
fromAST (AST.Mul x y) = Mul (fromAST x) (fromAST y)
fromAST (AST.Div x y) = Div (fromAST x) (fromAST y)
fromAST (AST.Ascribe x y) = fromAST x
fromAST (AST.IntLit i) = IntLit i

mkTest :: AST.AST -> JavaScriptAST -> JavaScriptAST
mkTest (AST.Variant x y) ident = do
  let test1 = Member ident (fromAST x)
  let test2 = mkTest y test1
  And test1 test2
mkTest (AST.OrderedPair x y@(AST.OrderedPair _ _)) it = do
  let test1 = mkTest x (Member it (IntLit 0))
  let test2 = mkTest y (Call (Member it (Identifier "slice")) [(IntLit 1)])
  And test1 test2
mkTest (AST.OrderedPair x y) it = do
  let test1 = mkTest x (Member it (IntLit 0))
  let test2 = mkTest y (Member it (IntLit 1))
  And test1 test2
mkTest (AST.UnorderedPair x y) it = do
  let test1 = mkTest x it
  let test2 = mkTest y it
  And test1 test2
mkTest (AST.Identifier x _) ident = Assign (Identifier x) ident
mkTest (AST.StrLit s) ident = Eq ident (StrLit s)
mkTest (AST.IntLit s) ident = Eq ident (IntLit s)

instance Aeson.ToJSON JavaScriptAST where
  toJSON (Identifier ident)
    = Aeson.object [
          "type" Aeson..= ("Identifier" :: Text.Text)
        , "name" Aeson..= ident
        ]
  toJSON (Function xs y)
    = Aeson.object [
          "type" Aeson..= ("FunctionExpression" :: Text.Text)
        , "params" Aeson..= xs
        , "body" Aeson..=
            Aeson.object [
                "type" Aeson..= ("BlockStatement" :: Text.Text)
              , "body" Aeson..= [
                    Aeson.object [
                        "type" Aeson..= ("ReturnStatement" :: Text.Text)
                    , "argument" Aeson..= y
                    ]]
              ]
        ]
  toJSON (Call x y)
    = Aeson.object [
          "type" Aeson..= ("CallExpression" :: Text.Text)
        , "callee" Aeson..= x
        , "arguments" Aeson..= y
        ]
  toJSON (Conditional x y z)
    = Aeson.object [
          "type" Aeson..= ("ConditionalExpression" :: Text.Text)
        , "test" Aeson..= x
        , "alternate" Aeson..= z
        , "consequent" Aeson..= y
      ]
  toJSON (Assign x y)
    = Aeson.object [
          "type" Aeson..= ("AssignmentExpression" :: Text.Text)
        , "operator" Aeson..= ("=" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Not x)
    = Aeson.object [
           "type" Aeson..= ("UnaryExpression" :: Text.Text)
         , "operator" Aeson..= ("!" :: Text.Text)
         , "argument" Aeson..= x
         ]
  toJSON (And x y)
    = Aeson.object [
          "type" Aeson..= ("BinaryExpression" :: Text.Text)
        , "operator" Aeson..= ("&&" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Or x y)
    = Aeson.object [
          "type" Aeson..= ("BinaryExpression" :: Text.Text)
        , "operator" Aeson..= ("||" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Eq x y)
    = Aeson.object [
          "type" Aeson..= ("BinaryExpression" :: Text.Text)
        , "operator" Aeson..= ("===" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Add x y)
    = Aeson.object [
          "type" Aeson..= ("BinaryExpression" :: Text.Text)
        , "operator" Aeson..= ("+" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Sub x y)
    = Aeson.object [
          "type" Aeson..= ("BinaryExpression" :: Text.Text)
        , "operator" Aeson..= ("-" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Mul x y)
    = Aeson.object [
          "type" Aeson..= ("BinaryExpression" :: Text.Text)
        , "operator" Aeson..= ("*" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Div x y)
    = Aeson.object [
          "type" Aeson..= ("BinaryExpression" :: Text.Text)
        , "operator" Aeson..= ("/" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Sequence x y)
    = Aeson.object [
          "type" Aeson..= ("SequenceExpression" :: Text.Text)
        , "expressions" Aeson..= [x, y]]
  toJSON (Member x y)
    = case y of
        Identifier _ -> Aeson.object [
            "type" Aeson..= ("MemberExpression" :: Text.Text)
          , "object" Aeson..= x
          , "property" Aeson..= y]
        _ ->  Aeson.object [
            "type" Aeson..= ("MemberExpression" :: Text.Text)
          , "object" Aeson..= x
          , "property" Aeson..= y
          , "computed" Aeson..= True ]
  toJSON (NTuple xs)
    = do
      Aeson.object [ "type" Aeson..= ("ArrayExpression" :: Text.Text)
                   , "elements" Aeson..= xs ]
  toJSON (Object properties)
    = do
        Aeson.object [ "type" Aeson..= ("ObjectExpression" :: Text.Text)
                       , "properties" Aeson..=
                           map (\(x,y) -> Aeson.object
                                          [ "type" Aeson..= ("Property" :: Text.Text)
                                          , "key" Aeson..= x
                                          , "value" Aeson..= y
                                          , "kind" Aeson..= ("init" :: Text.Text)
                                          ]) properties

                       ]
  toJSON (StrLit s)
    = Aeson.object [ "type" Aeson..= ("Literal" :: Text.Text)
                   , "value" Aeson..= s
                   ]
  toJSON (IntLit i)
    = Aeson.object [ "type" Aeson..= ("Literal" :: Text.Text)
                   , "value" Aeson..= i
                   ]
