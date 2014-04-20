{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}

module Language.LeatherScript.AST where

import qualified Data.Text                      as Text
import qualified Data.Vector                    as Vector
import qualified Data.Aeson                     as Aeson
import qualified Language.LeatherScript.Types()
import qualified Language.LeatherScript.Tokenizer as Tokenizer
import qualified Language.LeatherScript.Parser  as Parser

newtype Location = Location (Int, Int)
  deriving (Show)

data AST
  = Identifier Text.Text Location
  | Abstraction Text.Text AST
  | Application AST AST
  deriving (Show)

fromSyntaxTree :: Vector.Vector Tokenizer.Token -> Parser.SyntaxTree -> AST
fromSyntaxTree tokens (Parser.Token ident i) = do
  let tk = (Vector.!) tokens i
  Identifier ident (Location (Tokenizer.lineno tk, Tokenizer.columnno tk))
fromSyntaxTree tokens (Parser.Preference v) = do
  -- the code that couldn't be compiling by "cabal build"...
  -- Vector.foldl1 Application (Vector.map (fromSyntaxTree tokens) v)
    let f x ys = case ys of
                    [] -> x
                    _ -> do
                        let y = Vector.head ys
                        f (Application x y) (Vector.tail ys)

    let xs = Vector.map (fromSyntaxTree tokens) v
    f (Vector.head xs) (Vector.tail xs)

instance Aeson.ToJSON Location where
  toJSON (Location (lineno, columnno))
    = Aeson.object [
          "lineno" Aeson..= lineno
        , "columnno" Aeson..= columnno
        ]

instance Aeson.ToJSON AST where
  toJSON (Identifier ident loc)
    = Aeson.object [
          "type" Aeson..= ("Identifier" :: Text.Text)
        , "name" Aeson..= ident
        , "loc" Aeson..= loc
        ]
  toJSON (Abstraction param body)
    = Aeson.object [
          "type" Aeson..= ("Abstraction" :: Text.Text)
        , "param" Aeson..= param
        , "body" Aeson..= body
        ]
  toJSON (Application left right)
    = Aeson.object [
          "type" Aeson..= ("Application" :: Text.Text)
        , "left" Aeson..= left
        , "right" Aeson..= right
        ]
