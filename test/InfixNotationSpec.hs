{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}

import Test.Hspec

import           Control.Lens
import           Language.LeatherScript.Parser
import qualified Data.HashSet                   as HashSet
import qualified Data.HashMap.Strict            as HashMap

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
    it "(x + y) == (add x y)" $ do
      parse' ["x","+","y"] `shouldBe` Right (Preference [Token "add",Token "x",Token "y"])