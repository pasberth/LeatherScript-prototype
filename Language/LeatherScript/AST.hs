{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ViewPatterns               #-}

module Language.LeatherScript.AST where

import           Control.Applicative
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
  | Abstraction AST AST
  | Application AST AST
  | Conditional AST AST AST
  | Assign AST AST
  | Sequence AST AST
  deriving (Show)

fromSyntaxTree :: Vector.Vector Tokenizer.Token -> Parser.SyntaxTree -> AST
fromSyntaxTree tokens (Parser.Token ident i) = do
  let tk = (Vector.!) tokens i
  Identifier ident (Location (Tokenizer.lineno tk, Tokenizer.columnno tk))
fromSyntaxTree tokens (Parser.Preference v@(Vector.head -> Parser.Token "@LAMBDA" _))
  = case (fromSyntaxTree tokens <$> (Vector.!?) v 1, fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Abstraction x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference v@(Vector.head -> Parser.Token "@CONDITIONAL" _))
  = case (fromSyntaxTree tokens <$> (Vector.!?) v 1, fromSyntaxTree tokens <$> ((Vector.!?) v 2), fromSyntaxTree tokens <$> ((Vector.!?) v 3)) of
    (Just x, Just y, Just z) -> Conditional x y z
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference v@(Vector.head -> Parser.Token "@ASSIGN" _))
  = case (fromSyntaxTree tokens <$> (Vector.!?) v 1, fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Assign x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference v@(Vector.head -> Parser.Token "@SEQUENCE" _))
  = case (fromSyntaxTree tokens <$> (Vector.!?) v 1, fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Sequence x y
    _ -> reduceST tokens v

fromSyntaxTree tokens (Parser.Preference v) = reduceST tokens v
  -- the code that couldn't be compiling by "cabal build"...
  -- Vector.foldl1 Application (Vector.map (fromSyntaxTree tokens) v)
reduceST :: Vector.Vector Tokenizer.Token -> Vector.Vector Parser.SyntaxTree -> AST
reduceST tokens v = do
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
  toJSON (Conditional test alternate consequent)
    = Aeson.object [
          "type" Aeson..= ("Conditional" :: Text.Text)
        , "test" Aeson..= test
        , "alternate" Aeson..= alternate
        , "consequent" Aeson..= consequent
        ]
  toJSON (Assign left right)
    = Aeson.object [
          "type" Aeson..= ("Assign" :: Text.Text)
        , "left" Aeson..= left
        , "right" Aeson..= right
        ]
  toJSON (Sequence left right)
    = Aeson.object [
          "type" Aeson..= ("Sequence" :: Text.Text)
        , "left" Aeson..= left
        , "right" Aeson..= right
        ]