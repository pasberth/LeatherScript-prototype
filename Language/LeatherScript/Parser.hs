{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Language.LeatherScript.Parser where

import           Control.Lens                   hiding (Level)
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
  = Notation
    { _pattern :: Pattern
    , _replacement :: Replacement
    , _associativity :: Associativity
    , _level :: Level
    }
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
    { _keywords :: Keywords
    , _notations :: Notations
    , _notationStack :: NotationStack
    , _parserStack :: ParserStack
    , _tokens :: Vector.Vector Text.Text
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

makeLenses ''Notation
makeLenses ''ParserState

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

parse :: (Monad m, MonadIO m) => Vector.Vector Text.Text -> ParserT m (Either ParseError SyntaxTree)
parse _tokens = do
  tokens .= _tokens
  let rec = do
        parse1
        ParserState{_tokens} <- get
        case _tokens of
          [] -> do
            ParserState{_parserStack, _notationStack} <- get
            liftIO $ print _notationStack
            liftIO $ print _parserStack
            Vector.forM_ _notationStack $ \_ -> reduceLeft 
            ParserState{_parserStack, _notationStack} <- get
            liftIO $ print _notationStack
            liftIO $ print _parserStack
            return $ Right $ Vector.head _parserStack
          _ -> rec
  rec

takeOperand :: Monad m => ParserT m ()
takeOperand = do
  operands <- use (notationStack . element 0 . _2)
  operand <- uses parserStack Vector.head
  let newOperands = Vector.snoc operands operand
  notationStack %= (element 0 . _2 .~ newOperands)
  parserStack %= Vector.tail

reduceLeft :: Monad m => ParserT m ()
reduceLeft = do
  operands <- use (notationStack . element 0 . _2)
  operand <- uses parserStack Vector.head
  let newOperands = Vector.snoc operands operand
  (notation, _) <- uses notationStack Vector.head
  let e = mkEnvironment (notation ^. pattern) newOperands
  let st = subst (notation ^. replacement) e
  notationStack %= Vector.tail
  parserStack %= (element 0 .~ st)

parse1 :: MonadIO m => ParserT m ()
parse1 = do
  ParserState{_tokens, _keywords} <- get
  if
    | Vector.length _tokens == 0 -> do
      return ()
    | HashSet.member (Vector.head _tokens) _keywords -> do
      let kw = Vector.head _tokens
      ParserState{_notations} <- get
      case HashMap.lookup kw _notations of
        Just notation -> do
          case notation of
            Notation (Infix _ _ _) _ assoc level -> do
                  ParserState{_notationStack, _parserStack} <- get
              {-if
                | Vector.length notationStack == 0 -> do
                  let left = Vector.head parserStack
                  modify $ \s -> s { notationStack = [(notation, [left])] }
                  modify $ \s -> s { parserStack = Vector.tail parserStack }
                | otherwise -> do-}
                  foreach (Vector.toList _notationStack) $ \(leftNotation, appliedSTs) -> do
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
                      | otherwise -> do
                        exit
                  ParserState{_notationStack, _parserStack} <- get
                  let left = Vector.head _parserStack
                  modify $ \s -> s { _notationStack = Vector.cons (notation, [left]) _notationStack }
                  modify $ \s -> s { _parserStack = Vector.tail _parserStack }
                  modify $ \s -> s { _tokens = Vector.tail _tokens }
            Notation (Prefix _ _ _) _ assoc level -> do
                  ParserState{_notationStack, _parserStack} <- get
                  modify $ \s -> s { _notationStack = Vector.cons (notation, []) _notationStack }
                  modify $ \s -> s { _tokens = Vector.tail _tokens }
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
            ParserState{_parserStack, _notationStack} <- get
            foreach (Vector.toList _notationStack) $ \(notation, _) -> do
                                        let Notation pattern replacement assoc level = notation
                                        let kws = keywordsInPattern pattern
                                        if Vector.elem kw kws
                                          then exit
                                          else lift reduceLeft
            ParserState{_parserStack, _notationStack} <- get
            takeOperand
            modify $ \s -> s { _tokens = Vector.tail _tokens }

            --ParserState{parserStack, notationStack} <- get
            --liftIO $ print notationStack
            --liftIO $ print parserStack
          {-ParserState{notationStack, parserStack} <- get
          let (notation, appliedSTs) = Vector.head notationStack
          modify $ \s -> s { notationStack = Vector.cons (notation, Vector.snoc appliedSTs (Vector.head parserStack)) (Vector.tail notationStack) }
          case notation of
            Notation (Prefix _ _ _) _ _ _ -> do
              return ()-}
    | otherwise -> do
      ParserState{_parserStack} <- get
      let tk = Vector.head _tokens
      let st = Token tk
      modify $ \s -> s { _parserStack = Vector.cons st _parserStack }
      modify $ \s -> s { _tokens = Vector.tail _tokens }

st :: ParserState
st = ParserState {
       _keywords = HashSet.fromList ["*","+", "if", "then", "else"],
       _notations = HashMap.fromList [
              ("*", Notation (Infix "$x" [Keyword "*"] "$y") (Preference [Token "mul", Token "$x", Token "$y"]) LeftAssoc 70),
              ("+", Notation (Infix "$x" [Keyword "+"] "$y") (Preference [Token "add", Token "$x", Token "$y"]) LeftAssoc 60),
              ("if", Notation (Prefix "if" [Variable "$x", Keyword "then", Variable "$y", Keyword "else"] "$z") (Preference [Token "`if-then-else", Token "$x", Token "$y", Token "$z"]) RightAssoc 0)
              ],
      _notationStack = [],
      _parserStack = [],
      _tokens = []
}
