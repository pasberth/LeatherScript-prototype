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
                        ("+", Notation (Infix "$a" [Keyword "+"] "$b") (Preference [Token "add", Token "$a", Token "$b"]) LeftAssoc 60)
                    ,   ("-", Notation (Infix "$a" [Keyword "-"] "$b") (Preference [Token "sub", Token "$a", Token "$b"]) LeftAssoc 60)
                    ,   ("*", Notation (Infix "$a" [Keyword "*"] "$b") (Preference [Token "mul", Token "$a", Token "$b"]) LeftAssoc 70)
                    ,   ("/", Notation (Infix "$a" [Keyword "/"] "$b") (Preference [Token "div", Token "$a", Token "$b"]) LeftAssoc 70)
                    ]

main :: IO ()
main = hspec $ do
  describe "infix notations" $ do
    let parse' tokens = runParser (parse tokens) infixNotations
    it "x == x" $ do
      parse' ["x"] `shouldBe` Right (Token "x")
    it "x + y == (add x y)" $ do
      parse' ["x","+","y"] `shouldBe` Right (sexp "(add x y)")
    it "x + y + z == (add (add x y) z)" $ do
      parse' ["x","+","y","+","z"] `shouldBe` Right (sexp "(add (add x y) z)")