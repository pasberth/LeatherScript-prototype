{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE LambdaCase                 #-}

module Language.LeatherScript.Tokenizer where

import           Control.Arrow
import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Composition((.:))
import qualified Data.Char                      as Char
import qualified Data.Text                      as Text
import qualified Data.Vector                    as Vector
import qualified Language.LeatherScript.Types

data Token
  = Token
    { text :: Text.Text
    , lineno :: Int
    , columnno :: Int
    }
  deriving
    (Show)
type TokenDef = Vector.Vector Text.Text

newtype TokenizerT m a
  = TokenizerT
    {
      unTokenizerT :: ReaderT TokenDef m a
    }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader TokenDef
    )

type Tokenizer = TokenizerT Identity

runTokenizerT :: Monad m => TokenizerT m a -> TokenDef -> m a
runTokenizerT = runReaderT . unTokenizerT

runTokenizer :: Tokenizer a -> TokenDef -> a
runTokenizer = runIdentity .: runTokenizerT

token :: Text.Text -> Tokenizer (Maybe (Text.Text, Text.Text))
token "" = return Nothing
token text
  | Char.isSpace $ Text.head text = return $ Just $ Text.span Char.isSpace text
  | otherwise = do
      tokenDef <- ask
      case Vector.foldr (\def result -> result <|> (if Text.isPrefixOf def text then Just def else Nothing)) Nothing tokenDef of
        Just tk -> return $ Just (tk, Text.drop (Text.length tk) text)
        Nothing -> do
          let rec ""    = 0
              rec text1 = if Char.isSpace (Text.head text1) || Vector.any (\def -> Text.isPrefixOf def text1) tokenDef
                            then 0
                            else 1 + rec (Text.tail text1)
          return $ Just $ Text.splitAt (rec text) text

tokenize :: Text.Text -> Tokenizer (Vector.Vector Token)
tokenize text = go text 1 1 where
  go text lineno columnno = do
    token text >>= \case
      Just (tk, rest) -> do
        tks <- case reverse (Text.split (=='\n') tk) of
          [_] -> do
            go rest lineno (columnno + Text.length tk)
          (lastLine:headLines) -> do
            go rest (lineno + length headLines) (Text.length lastLine + 1)
        return $ Vector.cons (Token tk lineno columnno) tks
      Nothing -> do
        return []

tokenizeIgnoreSpaces :: Text.Text -> Tokenizer (Vector.Vector Token)
tokenizeIgnoreSpaces = tokenize >>>
  fmap (Vector.filter (\case
                    Token tk _ _ -> not $ Char.isSpace $ Text.head tk))