{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ViewPatterns               #-}

module Language.LeatherScript.AST where

import           Control.Applicative
import qualified Data.Char 
import qualified Data.Text                      as Text
import qualified Data.Vector                    as Vector
import qualified Data.Aeson                     as Aeson
import qualified Language.LeatherScript.Types()
import qualified Language.LeatherScript.Tokenizer as Tokenizer
import qualified Language.LeatherScript.Parser  as Parser

newtype Location = Location (FilePath, Int, Int)
  deriving (Show)

data AST
  = Identifier Text.Text Location
  | Abstraction AST AST
  | Application AST AST
  | Conditional AST AST AST
  | Assign AST AST
  | Sequence AST AST
  | Member AST AST
  | Variant AST AST
  | OrderedPair AST AST
  | UnorderedPair AST AST
  | Match AST [(AST, AST)]
  | StrLit Text.Text
  | IntLit Int
  | Not AST
  | And AST AST
  | Or AST AST
  | Eq AST AST
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST
  | Ascribe AST AST
  | SimpleType AST
  | EitherTy AST AST
  | TypeSynonym AST AST
  | Unit
  deriving (Show)

fromSyntaxTree :: Vector.Vector Tokenizer.Token -> Parser.SyntaxTree -> AST
fromSyntaxTree _ (Parser.Token "@UNIT" _) = Unit
fromSyntaxTree tokens (Parser.Token txt@(Text.head -> '"') _) = do
  let str = unescape $ Text.tail (Text.init txt)
  StrLit str where
    unescape "" = ""
    unescape x = case Text.head x of
                   '\\' -> Text.cons (Text.head (Text.tail x)) $ unescape (Text.tail (Text.tail x))
                   _ -> Text.cons (Text.head x) (unescape (Text.tail x))
fromSyntaxTree tokens (Parser.Token ident i) = do
  if Data.Char.isDigit (Text.head ident)
    then IntLit $ read $ Text.unpack ident
    else do
      let tk = (Vector.!) tokens i
      Identifier ident (Location (Tokenizer.filepath tk, Tokenizer.lineno tk, Tokenizer.columnno tk))
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@LAMBDA" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> (( Vector.!?) v 2)) of
    (Just x, Just y) -> Abstraction x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@CONDITIONAL" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)
         , fromSyntaxTree tokens <$> ((Vector.!?) v 3)) of
    (Just x, Just y, Just z) -> Conditional x y z
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@ASSIGN" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Assign x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@SEQUENCE" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Sequence x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@MEMBER" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Member x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@VARIANT" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Variant x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@ORDERED-PAIR" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> OrderedPair x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@UNORDERED-PAIR" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> UnorderedPair x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@MATCH" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1) of
    (Just x) -> Match x []
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@CASE" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)
         , fromSyntaxTree tokens <$> ((Vector.!?) v 3)) of
    (Just (Match x yzs), Just y, Just z) -> Match x (yzs ++ [(y,z)])
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@NOT" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1) of
    (Just x) -> Not x
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@AND" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> And x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@OR" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Or x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@EQ" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Eq x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@ADD" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Add x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@SUB" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Sub x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@MUL" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Mul x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@DIV" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
         , fromSyntaxTree tokens <$> ((Vector.!?) v 2)) of
    (Just x, Just y) -> Div x y
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@ASCRIBE" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
  , fromSyntaxTree tokens <$> (Vector.!?) v 2) of
           (Just x, Just y) -> Ascribe x y
           _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@UNIT" _))
  = Unit
fromSyntaxTree tokens (Parser.Preference
                        v@(Vector.head -> Parser.Token "@SIMPLE-TYPE" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1) of
    (Just x) -> SimpleType x
    _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@TYPE-SYNONYM" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
  , fromSyntaxTree tokens <$> (Vector.!?) v 2) of
           (Just x, Just y) -> TypeSynonym x y
           _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference
                       v@(Vector.head -> Parser.Token "@EITHER-TYPE" _))
  = case ( fromSyntaxTree tokens <$> (Vector.!?) v 1
  , fromSyntaxTree tokens <$> (Vector.!?) v 2) of
           (Just x, Just y) -> EitherTy x y
           _ -> reduceST tokens v
fromSyntaxTree tokens (Parser.Preference v) = reduceST tokens v
  -- the code that couldn't be compiling by "cabal build"...
  -- Vector.foldl1 Application (Vector.map (fromSyntaxTree tokens) v)
reduceST :: Vector.Vector Tokenizer.Token -> Vector.Vector Parser.SyntaxTree -> AST
reduceST tokens v = do
    let f x ys
          = case ys of
              [] -> x
              _ -> do let y = Vector.head ys
                      f (Application x y) (Vector.tail ys)

    let xs = Vector.map (fromSyntaxTree tokens) v
    f (Vector.head xs) (Vector.tail xs)

instance Aeson.ToJSON Location where
  toJSON (Location (filepath, lineno, columnno))
    = Aeson.object [
          "path" Aeson..= filepath
        , "lineno" Aeson..= lineno
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
  toJSON (Conditional test consequent alternate)
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
  toJSON (Member left right)
    = Aeson.object [
          "type" Aeson..= ("Member" :: Text.Text)
        , "left" Aeson..= left
        , "right" Aeson..= right
        ]
  toJSON (Variant tag value)
    = Aeson.object [ "type" Aeson..= ("Variant" :: Text.Text)
                   , "tag" Aeson..= tag
                   , "value" Aeson..= value
                   ]
  toJSON (OrderedPair left right)
    = Aeson.object [ "type" Aeson..= ("OrderedPair" :: Text.Text)
                   , "left" Aeson..= left
                   , "right" Aeson..= right
                   ]
  toJSON (UnorderedPair left right)
    = Aeson.object [ "type" Aeson..= ("UnorderedPair" :: Text.Text)
                   , "left" Aeson..= left
                   , "right" Aeson..= right
                   ]
  toJSON (Not argument)
    = Aeson.object [ "type" Aeson..= ("Not" :: Text.Text)
                   , "argument" Aeson..= argument
                   ]
  toJSON (And left right)
    = Aeson.object [ "type" Aeson..= ("And" :: Text.Text)
                   , "right" Aeson..= left
                   , "left" Aeson..= right
                   ]
  toJSON (Or left right)
    = Aeson.object [ "type" Aeson..= ("Or" :: Text.Text)
                   , "right" Aeson..= left
                   , "left" Aeson..= right
                   ]
  toJSON (Eq left right)
    = Aeson.object [ "type" Aeson..= ("Eq" :: Text.Text)
                   , "right" Aeson..= left
                   , "left" Aeson..= right
                   ]
  toJSON (Add left right)
    = Aeson.object [ "type" Aeson..= ("Add" :: Text.Text)
                   , "right" Aeson..= left
                   , "left" Aeson..= right
                   ]
  toJSON (Sub left right)
    = Aeson.object [ "type" Aeson..= ("Sub" :: Text.Text)
                   , "right" Aeson..= left
                   , "left" Aeson..= right
                   ]
  toJSON (Mul left right)
    = Aeson.object [ "type" Aeson..= ("Mul" :: Text.Text)
                   , "right" Aeson..= left
                   , "left" Aeson..= right
                   ]
  toJSON (Div left right)
    = Aeson.object [ "type" Aeson..= ("Div" :: Text.Text)
                   , "right" Aeson..= left
                   , "left" Aeson..= right
                   ]
  toJSON (Match x yzs)
    = Aeson.object [ "type" Aeson..= ("Match" :: Text.Text)
                   , "object" Aeson..= x
                   , "patterns" Aeson..= map (\(y,z) -> Aeson.object [ "left" Aeson..= y, "right" Aeson..= z ]) yzs
                   ]
  toJSON (StrLit s )
    = Aeson.object [ "type" Aeson..= ("String" :: Text.Text)
                   , "value" Aeson..= s
                   ]
  toJSON (IntLit i)
    = Aeson.object [ "type" Aeson..= ("integer" :: Text.Text)
                   , "value" Aeson..= i
                   ]
  toJSON (Ascribe x y)
    = Aeson.object [ "type" Aeson..= ("Ascribe" :: Text.Text)
                   , "left" Aeson..= x
                   , "right" Aeson..= y
                   ]
  toJSON (SimpleType s )
    = Aeson.object [ "type" Aeson..= ("SimpleType" :: Text.Text)
                   , "value" Aeson..= s
                   ]
  toJSON (TypeSynonym x y)
    = Aeson.object [ "type" Aeson..= ("TypeSynonym" :: Text.Text)
                   , "left" Aeson..= x
                   , "right" Aeson..= y
                   ]
  toJSON (EitherTy x y)
    = Aeson.object [ "type" Aeson..= ("EitherType" :: Text.Text)
                   , "left" Aeson..= x
                   , "right" Aeson..= y
                   ]
  toJSON (Unit)
    = Aeson.object [ "type" Aeson..= ("Unit" :: Text.Text)
                   ]
