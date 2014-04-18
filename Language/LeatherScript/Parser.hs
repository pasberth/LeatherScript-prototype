{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Language.LeatherScript.Parser where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
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
  = Notation Pattern Replacement Associativity Level
  deriving (Show)

type Keywords = HashSet.HashSet Keyword
type Notations = HashMap.HashMap Keyword Notation

type NotationStackValue = (Notation, Vector.Vector SyntaxTree)
type NotationStack = Vector.Vector NotationStackValue

data SyntaxTree
  = Token Text.Text
  | Preference (Vector.Vector SyntaxTree)

instance Show SyntaxTree where
  show (Token txt) = Text.unpack txt
  show (Preference v) = "(" ++ (join $ List.intersperse " " $ Vector.toList $ Vector.map show v) ++ ")"

data ParseError = ParseError
  deriving (Show)

type ParserStack = Vector.Vector SyntaxTree

data ParserState
  = ParserState
    { keywords :: Keywords
    , notations :: Notations
    , notationStack :: NotationStack
    , parserStack :: ParserStack
    , tokens :: Vector.Vector Text.Text
    }

newtype ParserT m a
  = ParserT
    {
      unParserT :: StateT ParserState m a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState ParserState
    , MonadIO
    )

type Parser = ParserT Identity

runParserT :: Monad m => ParserT m a -> ParserState -> m a
runParserT = evalStateT . unParserT

runParser :: Parser a -> ParserState -> a
runParser = runIdentity .: (evalStateT . unParserT)

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
countVariableInReplacement (Token (Text.uncons -> Just ('$', _))) = 1
countVariableInReplacement (Token _) = 0
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

mkEnvironment :: Pattern -> Vector.Vector SyntaxTree -> HashMap.HashMap Variable SyntaxTree
mkEnvironment pattern arguments = do
  let variables = variablesInPattern pattern
  HashMap.fromList $ Vector.toList $ Vector.zip variables arguments

subst :: Replacement -> HashMap.HashMap Variable SyntaxTree -> SyntaxTree
subst (Token v@(Text.uncons -> Just ('$', _))) e = Maybe.fromJust $ HashMap.lookup v e
subst st@(Token _) _ = st
subst (Preference v) e = Preference (Vector.map (\st -> subst st e) v)

{-variables :: Replacement -> Vector.Vector Variable
variables (Token (Text.uncons -> Just variable@('$', _))) = Vector.singleton variable
variables (Token _) = Vector.empty
variables (Preference v) = Vector.concatMap variables v
-}

parse :: MonadIO m => Vector.Vector Text.Text -> ParserT m (Either ParseError SyntaxTree)
parse tokens = do
  modify $ \s -> s { tokens = tokens }
  let rec = do
        parse1
        ParserState{tokens} <- get
        case tokens of
          [] -> do
            ParserState{parserStack, notationStack} <- get
            liftIO $ print notationStack
            liftIO $ print parserStack
            Vector.forM_ notationStack $ \_ -> reduceLeft 
            ParserState{parserStack, notationStack} <- get
            liftIO $ print notationStack
            liftIO $ print parserStack
            return $ Right $ Vector.head parserStack
          _ -> rec
  rec

reduceLeft :: Monad m => ParserT m ()
reduceLeft = do
  ParserState{notationStack, parserStack} <- get
  let (notation, operands) = Vector.head notationStack
  let operand = Vector.head parserStack
  let Notation pattern replacement _ _ = notation
  let e = mkEnvironment pattern (Vector.snoc operands operand)
  let st = subst replacement e
  modify $ \s -> s { notationStack = Vector.tail notationStack }
  modify $ \s -> s { parserStack = Vector.cons st (Vector.tail parserStack) }

parse1 :: Monad m => ParserT m ()
parse1 = do
  ParserState{tokens, keywords} <- get
  if
    | Vector.length tokens == 0 -> do
      return ()
    | HashSet.member (Vector.head tokens) keywords -> do
      let kw = Vector.head tokens
      ParserState{notations} <- get
      case HashMap.lookup kw notations of
        Just notation -> do
          case notation of
            Notation (Infix _ _ _) _ assoc level -> do
                  ParserState{notationStack, parserStack} <- get
              {-if
                | Vector.length notationStack == 0 -> do
                  let left = Vector.head parserStack
                  modify $ \s -> s { notationStack = [(notation, [left])] }
                  modify $ \s -> s { parserStack = Vector.tail parserStack }
                | otherwise -> do-}
                  foreach (Vector.toList notationStack) $ \(leftNotation, appliedSTs) -> do
                    let Notation pattern replacement _ _ = leftNotation
                    if
                      | (countVariableInPattern pattern - Vector.length appliedSTs) == 1 -> do
                        let Notation _ _ leftAssoc leftLevel = leftNotation
                        if
                          | leftLevel < level -> do
                            exit
                          | leftLevel > level -> do
                            lift reduceLeft
                          | otherwise -> do
                            lift reduceLeft
                  ParserState{notationStack, parserStack} <- get
                  let left = Vector.head parserStack
                  modify $ \s -> s { notationStack = Vector.cons (notation, [left]) notationStack }
                  modify $ \s -> s { parserStack = Vector.tail parserStack }
                  modify $ \s -> s { tokens = Vector.tail tokens }
            Notation (Prefix _ _ _) _ assoc level -> do
                  ParserState{notationStack, parserStack} <- get
                  modify $ \s -> s { notationStack = Vector.cons (notation, []) notationStack }
                  modify $ \s -> s { tokens = Vector.tail tokens }
                  {-let (leftNotation, appliedSTs) = Vector.last notationStack
                  let Notation pattern _ _ _ = leftNotation

                  if
                    | (countVariableInPattern pattern - Vector.length appliedSTs) == 1 -> do
                      let Notation _ _ leftAssoc leftLevel = leftNotation
                      if
                        | leftLevel < level -> do
                          let left = Vector.last parserStack
                          modify $ \s -> s { notationStack = Vector.snoc notationStack [(notation, [left])] }
                          modify $ \s -> s { parserStack = Vector.init parserStack }
                        | leftLevel > level -> do
                          undefined-}


        Nothing -> do
          undefined
    | otherwise -> do
      ParserState{parserStack} <- get
      let tk = Vector.head tokens
      let st = Token tk
      modify $ \s -> s { parserStack = Vector.cons st parserStack }
      modify $ \s -> s { tokens = Vector.tail tokens }

st :: ParserState
st = ParserState {
       keywords = HashSet.fromList ["*","+", "if"],
       notations = HashMap.fromList [
              ("*", Notation (Infix "$x" [Keyword "*"] "$y") (Preference [Token "mul", Token "$x", Token "$y"]) LeftAssoc 70),
              ("+", Notation (Infix "$x" [Keyword "+"] "$y") (Preference [Token "add", Token "$x", Token "$y"]) LeftAssoc 60),
              ("if", Notation (Prefix "if" [Variable "$x", Keyword "then", Variable "$y", Keyword "else"] "$z") (Preference [Token "`if-then-else", Token "$x", Token "$y", Token "$z"]) RightAssoc 0)
              ],
      notationStack = [],
      parserStack = [],
      tokens = []
}
