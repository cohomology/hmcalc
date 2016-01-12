module Main where

import Hmcalc.Lexer
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit 

-- | Simple Test of default "Show" instance for a complex TokenType
testTokenTypeShowComplex :: Test.HUnit.Test
testTokenTypeShowComplex = 
  TestCase (assertEqual "... deriving (Show)" "IdentifierToken \"bar\""
            (show $ IdentifierToken "bar"))

-- | Simple Test of default "Show" instance for a simple TokenType
testTokenTypeShowSimple :: Test.HUnit.Test
testTokenTypeShowSimple = 
  TestCase (assertEqual "... deriving (Show)" "UnderscoreToken"
            (show $ UnderscoreToken))

-- | Test that two TokenType's are equal, if and only if they are the same
testEqInstance :: Test.HUnit.Test 
testEqInstance = let types :: [TokenType]
                     types = [ IdentifierToken "foo", NumberToken "foo", Whitespaces "foo",
                               PlusToken, MinusToken, AsteriskToken, DivisionToken, LeftBracketToken,
                               RightBracketToken, LeftCurlyToken, RightCurlyToken, AssignmentToken,
                               CommaToken, PowerToken, TransposeToken, UnderscoreToken]
                     numbered :: [(TokenType, Int)]
                     numbered = zip types [1 ..]
                     cardProd :: [ ( (TokenType, Int), (TokenType, Int) ) ]
                     cardProd = [ (x,y) | x <- numbered, y <- numbered ]
                     testProd :: ( (TokenType, Int), (TokenType, Int) ) -> Assertion
                     testProd ((a,b),(c,d)) = assertEqual ("testEqInstance " ++ (show a) ++ (show b))
                                                          ( b == d ) ( a == c ) 
                 in TestList $ map (TestCase . testProd) cardProd

-- | List of all tests contained in this file
allTests :: [Test.Framework.Test]
allTests = hUnitTestToTests $ TestList [
   TestLabel "... deriving (Show) Complex" testTokenTypeShowComplex,
   TestLabel "... deriving (Show) Simple" testTokenTypeShowSimple,
   TestLabel "... deriving (Eq)" testEqInstance
 ]

-- | Run unit tests
main :: IO ()
main = defaultMain allTests 
