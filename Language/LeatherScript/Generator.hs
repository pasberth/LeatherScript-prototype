{-# LANGUAGE OverloadedStrings          #-}

module Language.LeatherScript.Generator where

import qualified Data.Text                      as Text
import qualified Data.Aeson                     as Aeson
import qualified Language.LeatherScript.AST     as AST

data JavaScriptAST
  = Identifier Text.Text
  | Call JavaScriptAST JavaScriptAST

fromAST :: AST.AST -> JavaScriptAST
fromAST (AST.Identifier ident _) = Identifier ident
fromAST (AST.Application x y) = Call (fromAST x) (fromAST y)

instance Aeson.ToJSON JavaScriptAST where
  toJSON (Identifier ident)
    = Aeson.object [
          "type" Aeson..= ("Identifier" :: Text.Text)
        , "name" Aeson..= ident
        ]
  toJSON (Call x y)
    = Aeson.object [
          "type" Aeson..= ("CallExpression" :: Text.Text)
        , "callee" Aeson..= x
        , "arguments" Aeson..= [y]
        ]