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

prefixNotations :: ParserState
prefixNotations
  = emptyParserState
    & keywords .~ HashSet.fromList ["~", "if", "then", "else"]
    & notations .~ HashMap.fromList [
                      ("~", Notation (Prefix "~" [] "$a") (sexp "(~ $a)") RightAssoc 35)
                    , ("if", Notation (Prefix "if" [Variable "$a", Keyword "then", Variable "$b", Keyword "else"] "$c") (sexp "(if-then-else $a $b $c)") RightAssoc 0)
                    ]

postfixNotations :: ParserState
postfixNotations
  = emptyParserState
    & keywords .~ HashSet.fromList ["!"]
    & notations .~ HashMap.fromList [
                      ("!", Notation (Postfix "$a" [] "!") (sexp "(! $a)") LeftAssoc 80)
                    ]

outfixNotations :: ParserState
outfixNotations
  = emptyParserState
    & keywords .~ HashSet.fromList ["(", ")"]
    & notations .~ HashMap.fromList [
                    ("(", Notation (Outfix "(" [Variable "$a"] ")") (sexp "$a") NoAssoc 200)
                  ]

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

complexNotations :: ParserState
complexNotations
  = emptyParserState
    & keywords .~ HashSet.unions [
                      prefixNotations ^. keywords
                    , postfixNotations ^. keywords
                    , outfixNotations ^. keywords
                    , infixNotations ^. keywords
                    ]
    & notations .~ HashMap.unions [
                      prefixNotations ^. notations
                    , postfixNotations ^. notations
                    , outfixNotations ^. notations
                    , infixNotations ^. notations
                    ]

main :: IO ()
main = hspec $ do
  describe "prefix notations" $ do
    let parse' tokens = runParser (parse tokens) prefixNotations
    let assert x y = parse' (tokenize x) `shouldBe` Right (sexp y)
    let failure x y = parse' (tokenize x) `shouldBe` Left y

    it "~ P == (~ P)" $ do
      assert "~ P" "(~ P)"

    it "~ ~ P == (~ (~ P))" $ do
      assert "~ ~ P" "(~ (~ P))"

    it "~ ~ ~ P == (~ (~ (~ P)))" $ do
      assert "~ ~ ~ P" "(~ (~ (~ P)))"

    it "if a then b else c == (if-then-else a b c)" $ do
      assert "if a then b else c" "(if-then-else a b c)"

    it "if ~ a then ~ b else ~ c == (if-then-else (~ a) (~ b) (~ c))" $ do
      assert "if ~ a then ~ b else ~ c" "(if-then-else (~ a) (~ b) (~ c))"

    it "if if a then b else c then if d then e else f else if g then h else i == (if-then-else (if-then-else a b c) (if-then-else d e f) (if-then-else g h i))" $ do
      assert "if if a then b else c then if d then e else f else if g then h else i" "(if-then-else (if-then-else a b c) (if-then-else d e f) (if-then-else g h i))"

    it "if a else b then c -> parse error" $ do
      failure "if a else b then c" (Unexpected "else")

  describe "postfix notations" $ do
    let parse' tokens = runParser (parse tokens) postfixNotations
    let assert x y = parse' (tokenize x) `shouldBe` Right (sexp y)

    it "a ! == (! a)" $ do
      assert "a !" "(! a)"

    it "a ! ! == (! (! a))" $ do
      assert "a ! !" "(!(! a))"

    it "a ! ! ! == (! (! (! a)))" $ do
      assert "a ! ! !" "(! (! (! a)))"

  describe "outfix notations" $ do
    let parse' tokens = runParser (parse tokens) outfixNotations
    let assert x y = parse' (tokenize x) `shouldBe` Right (sexp y)

    it "(a) == (a)" $ do
      assert "( a )" "a"
    it "((a)) == (a)" $ do
      assert "( ( a ) )" "a"
    it "(((a))) == (a)" $ do
      assert "( ( ( a ) ) )" "a"

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
      assert "x and y and z" "(and x (and y z))"
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

  describe "complex notations" $ do
    let parse' tokens = runParser (parse tokens) complexNotations
    let assert x y = parse' (tokenize x) `shouldBe` Right (sexp y)
    it "(a + b) == (+ a b)" $ do
      assert "( a + b )" "(+ a b)"
    it "a * (b + c) == (* a (+ b c))" $ do
      assert "a * ( b + c )" "(* a (+ b c))"
    it "~ a = b == (~ (= a b))" $ do
      assert "~ a = b" "(~ (= a b))"
    it "if a and b then c + d else e + f == (if-then-else (and a b) (+ c d) (+ e f))" $ do
      assert "if a and b then c + d else e + f" "(if-then-else (and a b) (+ c d) (+ e f))"
    it "~ a ! + ~ b ! == (~ (+ (! a) (~ (! b))))" $ do
      assert "~ a ! + ~ b !" "(~ (+ (! a) (~ (! b))))"