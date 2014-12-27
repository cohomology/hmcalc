module Main where

import Hmcalc.Lexer
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit 

testMatchIdentifier1  = TestCase (assertEqual "hugo" (Just Token { tokenType=IdentifierToken, tokenLength=4, 
                                                                   tokenPosition=0 }) 
                                   (matchIdentifier "hugo" 0))
testMatchIdentifier2  = TestCase (assertEqual "ugo" (Just Token { tokenType=IdentifierToken, 
                                                                  tokenLength=3, tokenPosition=1 }) 
                                   (matchIdentifier "hugo" 1))
testMatchIdentifier3  = TestCase (assertEqual "go" (Just Token { tokenType=IdentifierToken, 
                                                                 tokenLength=2, tokenPosition=2 }) 
                                   (matchIdentifier "hugo" 2))
testMatchIdentifier4  = TestCase (assertEqual "o" (Just Token { tokenType=IdentifierToken, 
                                                                tokenLength=1, tokenPosition=3 }) 
                                   (matchIdentifier "hugo" 3))
testMatchIdentifier5  = TestCase (assertEqual "<Nothing>" (Nothing) 
                                   (matchIdentifier "hugo" 4))
testMatchIdentifier6  = TestCase (assertEqual "hello" (Just Token { tokenType=IdentifierToken, 
                                                                    tokenLength=5, tokenPosition=0 }) 
                                   (matchIdentifier "hello welt" 0))
testMatchIdentifier7  = TestCase (assertEqual "hello" (Nothing) (matchIdentifier "hello welt" 22))
testMatchIdentifier8  = TestCase (assertEqual "hello" (Nothing) (matchIdentifier "" 0))
testMatchIdentifier9  = TestCase (assertEqual "hello" (Nothing) (matchIdentifier "" 1))
testMatchIdentifier10 = TestCase (assertEqual "1hugo" (Nothing) (matchIdentifier "1hugo" 0))
testMatchIdentifier11 = TestCase (assertEqual "hugo" (Just Token { tokenType=IdentifierToken, 
                                                                   tokenLength=4, tokenPosition=1 }) 
                                   (matchIdentifier "1hugo" 1))
testMatchIdentifier12 = TestCase (assertEqual "+hugo" (Nothing) (matchIdentifier "+hugo" 0))
testMatchIdentifier13 = TestCase (assertEqual "hugo" (Just Token { tokenType=IdentifierToken, 
                                                                   tokenLength=4, tokenPosition=1 }) 
                                   (matchIdentifier "+hugo" 1))
testMatchIdentifier14 = TestCase (assertEqual "+" (Nothing) (matchIdentifier "+" 0))
testMatchIdentifier15 = TestCase (assertEqual "-" (Nothing) (matchIdentifier "-" 0))
testMatchIdentifier16 = TestCase (assertEqual "*" (Nothing) (matchIdentifier "*" 0))
testMatchIdentifier17 = TestCase (assertEqual "/" (Nothing) (matchIdentifier "/" 0))
testMatchIdentifier18 = TestCase (assertEqual "(" (Nothing) (matchIdentifier "(" 0))
testMatchIdentifier19 = TestCase (assertEqual ")" (Nothing) (matchIdentifier ")" 0))
testMatchIdentifier20 = TestCase (assertEqual "{" (Nothing) (matchIdentifier "{" 0))
testMatchIdentifier21 = TestCase (assertEqual "}" (Nothing) (matchIdentifier "}" 0))
testMatchIdentifier22 = TestCase (assertEqual "=" (Nothing) (matchIdentifier "=" 0))
testMatchIdentifier23 = TestCase (assertEqual "." (Nothing) (matchIdentifier "." 0))
testMatchIdentifier24 = TestCase (assertEqual "0" (Nothing) (matchIdentifier "0" 0))
testMatchIdentifier25 = TestCase (assertEqual "1" (Nothing) (matchIdentifier "1" 0))
testMatchIdentifier26 = TestCase (assertEqual "2" (Nothing) (matchIdentifier "2" 0))
testMatchIdentifier27 = TestCase (assertEqual "9" (Nothing) (matchIdentifier "9" 0))
testMatchIdentifier28 = TestCase (assertEqual "HUgO22" (Just Token { tokenType=IdentifierToken, tokenLength=6, 
                                                                     tokenPosition=0 }) 
                                   (matchIdentifier "HUgO22" 0))
testMatchIdentifier29 = TestCase (assertEqual "Ü"  (Just Token { tokenType=IdentifierToken, tokenLength=1, 
                                                                 tokenPosition=0 }) 
                                   (matchIdentifier "Ü" 0))

tests = hUnitTestToTests $ TestList [
                    TestLabel "matchIdentifier Test1"   testMatchIdentifier1,
                    TestLabel "matchIdentifier Test2"   testMatchIdentifier2,
                    TestLabel "matchIdentifier Test3"   testMatchIdentifier3,
                    TestLabel "matchIdentifier Test4"   testMatchIdentifier4,
                    TestLabel "matchIdentifier Test5"   testMatchIdentifier5,
                    TestLabel "matchIdentifier Test6"   testMatchIdentifier6,
                    TestLabel "matchIdentifier Test7"   testMatchIdentifier7,
                    TestLabel "matchIdentifier Test8"   testMatchIdentifier8,
                    TestLabel "matchIdentifier Test9"   testMatchIdentifier9,
                    TestLabel "matchIdentifier Test10"  testMatchIdentifier10,
                    TestLabel "matchIdentifier Test11"  testMatchIdentifier11,
                    TestLabel "matchIdentifier Test12"  testMatchIdentifier12,
                    TestLabel "matchIdentifier Test13"  testMatchIdentifier13,
                    TestLabel "matchIdentifier Test14"  testMatchIdentifier14,
                    TestLabel "matchIdentifier Test15"  testMatchIdentifier15,
                    TestLabel "matchIdentifier Test16"  testMatchIdentifier16,
                    TestLabel "matchIdentifier Test17"  testMatchIdentifier17,
                    TestLabel "matchIdentifier Test18"  testMatchIdentifier18,
                    TestLabel "matchIdentifier Test19"  testMatchIdentifier19,
                    TestLabel "matchIdentifier Test20"  testMatchIdentifier20,
                    TestLabel "matchIdentifier Test21"  testMatchIdentifier21,
                    TestLabel "matchIdentifier Test22"  testMatchIdentifier22,
                    TestLabel "matchIdentifier Test23"  testMatchIdentifier23,
                    TestLabel "matchIdentifier Test24"  testMatchIdentifier24,
                    TestLabel "matchIdentifier Test25"  testMatchIdentifier25,
                    TestLabel "matchIdentifier Test26"  testMatchIdentifier26,
                    TestLabel "matchIdentifier Test27"  testMatchIdentifier27,
                    TestLabel "matchIdentifier Test28"  testMatchIdentifier28,
                    TestLabel "matchIdentifier Test29"  testMatchIdentifier29
                 ]

main = defaultMain tests 
