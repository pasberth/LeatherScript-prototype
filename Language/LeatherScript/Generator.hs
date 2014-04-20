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
  | While JavaScriptAST JavaScriptAST
  | Assign JavaScriptAST JavaScriptAST
  | Sequence JavaScriptAST JavaScriptAST

fromAST :: AST.AST -> JavaScriptAST
fromAST (AST.Identifier ident _) = Identifier ident
fromAST (AST.Application x y) = Call (fromAST x) (fromAST y)
fromAST (AST.Abstraction x y) = Function (fromAST x) (fromAST y)
fromAST (AST.Conditional x y z) = Conditional (fromAST x) (fromAST y) (fromAST z)
fromAST (AST.Assign x y) = Assign (fromAST x) (fromAST y)
fromAST (AST.Sequence x y) = Sequence (fromAST x) (fromAST y)

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
        , "alternate" Aeson..= y
        , "consequent" Aeson..= z
      ]
  toJSON (Assign x y)
    = Aeson.object [
          "type" Aeson..= ("AssignmentExpression" :: Text.Text)
        , "operator" Aeson..= ("=" :: Text.Text)
        , "left" Aeson..= x
        , "right" Aeson..= y
        ]
  toJSON (Sequence x y)
    = Aeson.object [
          "type" Aeson..= ("SequenceExpression" :: Text.Text)
        , "expressions" Aeson..= [x, y]]