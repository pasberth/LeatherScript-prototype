{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}

import Test.Hspec

import           Control.Applicative
import           Control.Lens
import           Language.LeatherScript.Parser
import qualified Data.Text                      as Text
import qualified Data.Vector                    as Vector
import qualified Data.HashSet                   as HashSet
import qualified Data.HashMap.Strict            as HashMap
import qualified Text.Trifecta
import qualified Text.Trifecta.Delta

tokenize :: String -> Vector.Vector Text.Text
tokenize s = case Text.Trifecta.parseString tks (Text.Trifecta.Delta.Columns 0 0) s of
    Text.Trifecta.Success x -> x
    Text.Trifecta.Failure e -> error (show e)
  where
    tks = Vector.fromList <$> many (Text.Trifecta.spaces *> tk <* Text.Trifecta.spaces)
    tk = Text.pack <$> some (Text.Trifecta.noneOf " ")

sexp :: String -> SyntaxTree
sexp s = case Text.Trifecta.parseString parser (Text.Trifecta.Delta.Columns 0 0) s of
    Text.Trifecta.Success x -> x
    Text.Trifecta.Failure e -> error (show e)
  where
    parser = Text.Trifecta.spaces *> (pref <|> token) <* Text.Trifecta.spaces
    pref = do
      Text.Trifecta.char '('
      xs <- many parser
      Text.Trifecta.char ')'
      return $ Preference $ Vector.fromList xs
    token = do
      s <- some $ Text.Trifecta.noneOf "() "
      return $ Token $ Text.pack s

infixNotations :: ParserState
infixNotations
  = emptyParserState
    & keywords .~ HashSet.fromList ["+","-","*","/"]
    & notations .~ HashMap.fromList [
                        ("+", Notation (Infix "$a" [Keyword "+"] "$b") (Preference [Token "+", Token "$a", Token "$b"]) LeftAssoc 60)
                    ,   ("-", Notation (Infix "$a" [Keyword "-"] "$b") (Preference [Token "-", Token "$a", Token "$b"]) LeftAssoc 60)
                    ,   ("*", Notation (Infix "$a" [Keyword "*"] "$b") (Preference [Token "*", Token "$a", Token "$b"]) LeftAssoc 70)
                    ,   ("/", Notation (Infix "$a" [Keyword "/"] "$b") (Preference [Token "/", Token "$a", Token "$b"]) LeftAssoc 70)
                    ]

main :: IO ()
main = hspec $ do
  describe "infix notations" $ do
    let parse' tokens = runParser (parse tokens) infixNotations
    let assert x y = parse' (tokenize x) `shouldBe` Right (sexp y)
    it "a == a" $ do
      assert "a" "a"
    it "a + b == (+ a b)" $ do
      assert "a + b" "(+ a b)"
    it "a + b + c == (+ (+ a b) c)" $ do
      assert "a + b + c" "(+ (+ a b) c)"
    it "(a + b * c + d) == (+ (+ a (* b c)) d)" $ do
      assert "a + b * c + d" "(+ (+ a (* b c)) d)"
    it "(a * b + c + d) == (+ (+ (* a b) c) d)" $ do
      assert "a * b + c + d" "(+ (+ (* a b) c) d)"
