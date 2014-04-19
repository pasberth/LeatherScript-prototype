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
    & keywords .~ HashSet.fromList ["+","-","*","/","=","and","or"]
    & notations .~ HashMap.fromList [
                        ("+", Notation (Infix "$a" [Keyword "+"] "$b") (sexp "(+ $a $b)") LeftAssoc 60)
                    ,   ("-", Notation (Infix "$a" [Keyword "-"] "$b") (sexp "(- $a $b)") LeftAssoc 60)
                    ,   ("*", Notation (Infix "$a" [Keyword "*"] "$b") (sexp "(* $a $b)") LeftAssoc 70)
                    ,   ("/", Notation (Infix "$a" [Keyword "/"] "$b") (sexp "(/ $a $b)") LeftAssoc 70)
                    ,   ("=", Notation (Infix "$a" [Keyword "="] "$b") (sexp "(= $a $b)") NoAssoc 40)
                    ,   ("and", Notation (Infix "$a" [Keyword "and"]"$b") (sexp "(and $a $b)") RightAssoc 30)
                    ,   ("or", Notation (Infix "$a" [Keyword "or"] "$b") (sexp "(or $a $b)") RightAssoc 20)
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
    it "(a + b + c * d) == (+ (+ a b) (* c d))" $ do
      assert "a + b + c * d" "(+ (+ a b) (* c d))"
    it "(a * b + c * d) == (+ (* a b) (* c d))" $ do
      assert "a * b + c * d" "(+ (* a b) (* c d))"
    it "(a + b * c * d) == (+ a (* (* b c) d))" $ do
      assert "a + b * c * d" "(+ a (* (* b c) d))"
    it "(a * b * c + d) == (+ (* (* a b) c) d)" $ do
      assert "a * b * c + d" "(+ (* (* a b) c) d)"
    it "(x and y) == (and x y)" $ do
      assert "x and y" "(and x y)"
    it "(x and y and z) == (and x (and y z))" $ do
      assert "(x and y and z)" "(and x (and y z))"
    it "(a = b and c = d) == (and (= a b) (= c d))" $ do
      assert "a = b and c = d" "(and (= a b) (= c d))"
    -- TODO: a = b = c
    it "(a or b and c or d) == (or a (or (and b c) d))" $ do
      assert "a or b and c or d" "(or a (or (and b c) d))"
    it "(a and b or c or d) == (or (and a b) (or c d))" $ do
      assert "a and b or c or d" "(or (and a b) (or c d))"
    it "(a or b or c and d) == (or a (or b (and c d)))" $ do
      assert "a or b or c and d" "(or a (or b (and c d)))"
    it "(a and b or c and d) == (or (and a b) (and c d))" $ do
      assert "a and b or c and d" "(or (and a b) (and c d))"
    it "(a or b and c and d) == (or a (and b (and c d)))" $ do
      assert "a or b and c and d" "(or a (and b (and c d)))"
    it "(a and b and c or d) == (or (and a (and b c)) d)" $ do
      assert "a and b and c or d" "(or (and a (and b c)) d)"
