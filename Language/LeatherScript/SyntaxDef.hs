module Language.LeatherScript.SyntaxDef where

import           Control.Arrow
import           Control.Applicative
import           Control.Lens
import qualified Data.Text                      as Text
import qualified Data.Vector                    as Vector
import qualified Data.HashSet                   as HashSet
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.IORef                     as IORef
import qualified Text.Trifecta
import           Language.LeatherScript.Tokenizer as Tokenizer
import           Language.LeatherScript.Parser as Parser

data SyntaxDef
  = TokenDef Text.Text
  | NotationDef Text.Text Text.Text Int Associativity

parseToken :: Text.Trifecta.Parser SyntaxDef
parseToken = TokenDef . Text.pack <$ Text.Trifecta.symbol "Token" <*> Text.Trifecta.stringLiteral

parseNotation :: Text.Trifecta.Parser SyntaxDef
parseNotation = (\x y -> NotationDef (Text.pack x) (Text.pack y)) <$ Text.Trifecta.symbol "Notation" <*> Text.Trifecta.stringLiteral <* Text.Trifecta.symbol ":=" <*> Text.Trifecta.stringLiteral <*> parseLevel <*> parseAssociativity

parseLevel :: Text.Trifecta.Parser Int
parseLevel = fromInteger <$ Text.Trifecta.symbol "level" <*> Text.Trifecta.natural

parseAssociativity :: Text.Trifecta.Parser Associativity
parseAssociativity = (l <|> r <|> no) <* Text.Trifecta.symbol "associativity" where
  l = LeftAssoc <$ Text.Trifecta.symbol "left"
  r = RightAssoc <$ Text.Trifecta.symbol "right"
  no = NoAssoc <$ Text.Trifecta.symbol "no"

parseSyntaxDef :: Text.Trifecta.Parser (Vector.Vector SyntaxDef)
parseSyntaxDef = Vector.fromList <$> many (parseToken <|> parseNotation)

parseText :: TokenDef -> Keywords -> Notations -> Text.Text -> Either ParseError SyntaxTree
parseText tokenDef_ keywords_ notations_ source = do
  let tokens = runTokenizer (tokenizeIgnoreSpaces source) tokenDef_
  let tokens1 = Vector.map (\(Tokenizer.Token tk _ _) -> tk) tokens
  let parserState = emptyParserState & keywords .~ keywords_ & notations .~ notations_
  runParser (parse tokens1) parserState

mkParserFromFile :: FilePath -> IO (Maybe (TokenDef, Keywords, Notations))
mkParserFromFile path = do
  maybeVec <- Text.Trifecta.parseFromFile parseSyntaxDef path
  case maybeVec of
    Nothing -> return Nothing
    Just vec -> do
      tokenDef <- IORef.newIORef Vector.empty
      syntaxs <- IORef.newIORef (HashSet.empty, HashMap.empty)
      Vector.forM_ vec $ \x -> case x of
        TokenDef tk -> do
          tkDef <- IORef.readIORef tokenDef
          IORef.writeIORef tokenDef (Vector.cons tk tkDef)
        NotationDef x y level assoc -> do
          tkDef <- IORef.readIORef tokenDef
          sxDef <- IORef.readIORef syntaxs
          let pattern = patternFromVector (Vector.map (\(Tokenizer.Token tk _ _) -> tk) (runTokenizer (tokenizeIgnoreSpaces x) tkDef))
          case parseText tkDef (fst sxDef) (snd sxDef) y of
            Left err -> print err
            Right replacement -> do
              let notation = Notation pattern replacement assoc level
              let kws = keywordsInPattern pattern
              let kw = if Vector.length kws == 0 then Text.pack "" else Vector.head kws
              let newSxDef = (HashSet.union (HashSet.fromList (Vector.toList kws)) (fst sxDef), HashMap.insert kw notation (snd sxDef))
              IORef.writeIORef syntaxs newSxDef
      tkDef <- IORef.readIORef tokenDef
      sxDef <- IORef.readIORef syntaxs
      return $ Just (tkDef, fst sxDef, snd sxDef)
