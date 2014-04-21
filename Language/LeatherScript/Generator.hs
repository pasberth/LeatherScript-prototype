{-# LANGUAGE OverloadedStrings          #-}

module Language.LeatherScript.Generator where

import qualified Data.Text                      as Text
import qualified Data.Aeson                     as Aeson
import qualified Language.LeatherScript.AST     as AST

data JavaScriptAST
  = Identifier Text.Text
  | Function JavaScriptAST JavaScriptAST
  | Call JavaScriptAST JavaScriptAST
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
  | Object [(JavaScriptAST, JavaScriptAST)]
  | StrLit Text.Text

fromAST :: AST.AST -> JavaScriptAST
fromAST (AST.Identifier ident _) = Identifier ident
fromAST (AST.Application x y) = Call (fromAST x) (fromAST y)
fromAST (AST.Abstraction x y) = Function (fromAST x) (fromAST y)
fromAST (AST.Conditional x y z) = Conditional (fromAST x) (fromAST y) (fromAST z)
fromAST (AST.Assign x y) = Assign (fromAST x) (fromAST y)
fromAST (AST.Sequence x y) = Sequence (fromAST x) (fromAST y)
fromAST (AST.Member x y) = Member (fromAST x) (fromAST y)
fromAST (AST.Variant x y) = Object [(fromAST x, fromAST y)]
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

mkTest :: AST.AST -> JavaScriptAST -> JavaScriptAST
mkTest (AST.Variant x y) ident = do
  let test1 = Member ident (fromAST x)
  let test2 = mkTest y test1
  test2
mkTest (AST.UnorderedPair x y) it = do
  let test1 = mkTest x it
  let test2 = mkTest y it
  And test1 test2
mkTest (AST.Identifier x _) ident = Assign (Identifier x) ident
mkTest (AST.StrLit s) ident = Eq ident (StrLit s)

instance Aeson.ToJSON JavaScriptAST where
  toJSON (Identifier ident)
    = Aeson.object [
          "type" Aeson..= ("Identifier" :: Text.Text)
        , "name" Aeson..= ident
        ]
  toJSON (Function x y)
    = Aeson.object [
          "type" Aeson..= ("FunctionExpression" :: Text.Text)
        , "params" Aeson..= [x]
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
        , "arguments" Aeson..= [y]
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
    = Aeson.object [
          "type" Aeson..= ("MemberExpression" :: Text.Text)
        , "object" Aeson..= x
        , "property" Aeson..= y]
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
