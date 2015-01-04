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


testMatchSingleChars1 = TestCase (assertEqual "a+b" (Nothing) (matchSingleChars "a+b" 0))
testMatchSingleChars2 = TestCase (assertEqual "a+b" 
  (Just Token { tokenType=PlusToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a+b" 1))
testMatchSingleChars3 = TestCase (assertEqual "a+b" (Nothing) (matchSingleChars "a+b" 2))
testMatchSingleChars4 = TestCase (assertEqual "a+b" (Nothing) (matchSingleChars "a+b" 5))

testMatchSingleChars5 = TestCase (assertEqual "a-b" (Nothing) (matchSingleChars "a-b" 0))
testMatchSingleChars6 = TestCase (assertEqual "a-b" 
  (Just Token { tokenType=MinusToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a-b" 1))
testMatchSingleChars7 = TestCase (assertEqual "a-b" (Nothing) (matchSingleChars "a-b" 2))
testMatchSingleChars8 = TestCase (assertEqual "a-b" (Nothing) (matchSingleChars "a-b" 5))

testMatchSingleChars9 = TestCase (assertEqual "a*b" (Nothing) (matchSingleChars "a*b" 0))
testMatchSingleChars10 = TestCase (assertEqual "a*b" 
  (Just Token { tokenType=AsteriskToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a*b" 1))
testMatchSingleChars11 = TestCase (assertEqual "a*b" (Nothing) (matchSingleChars "a*b" 2))
testMatchSingleChars12 = TestCase (assertEqual "a*b" (Nothing) (matchSingleChars "a*b" 5))

testMatchSingleChars13 = TestCase (assertEqual "a/b" (Nothing) (matchSingleChars "a/b" 0))
testMatchSingleChars14 = TestCase (assertEqual "a/b" 
  (Just Token { tokenType=DivisionToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a/b" 1))
testMatchSingleChars15 = TestCase (assertEqual "a/b" (Nothing) (matchSingleChars "a/b" 2))
testMatchSingleChars16 = TestCase (assertEqual "a/b" (Nothing) (matchSingleChars "a/b" 5))

testMatchSingleChars17 = TestCase (assertEqual "a(b" (Nothing) (matchSingleChars "a(b" 0))
testMatchSingleChars18 = TestCase (assertEqual "a(b" 
  (Just Token { tokenType=LeftBracketToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a(b" 1))
testMatchSingleChars19 = TestCase (assertEqual "a(b" (Nothing) (matchSingleChars "a(b" 2))
testMatchSingleChars20 = TestCase (assertEqual "a(b" (Nothing) (matchSingleChars "a(b" 5))

testMatchSingleChars21 = TestCase (assertEqual "a)b" (Nothing) (matchSingleChars "a)b" 0))
testMatchSingleChars22 = TestCase (assertEqual "a)b" 
  (Just Token { tokenType=RightBracketToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a)b" 1))
testMatchSingleChars23 = TestCase (assertEqual "a)b" (Nothing) (matchSingleChars "a)b" 2))
testMatchSingleChars24 = TestCase (assertEqual "a)b" (Nothing) (matchSingleChars "a)b" 5))

testMatchSingleChars25 = TestCase (assertEqual "a{b" (Nothing) (matchSingleChars "a{b" 0))
testMatchSingleChars26 = TestCase (assertEqual "a{b" 
  (Just Token { tokenType=LeftCurlyToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a{b" 1))
testMatchSingleChars27 = TestCase (assertEqual "a{b" (Nothing) (matchSingleChars "a{b" 2))
testMatchSingleChars28 = TestCase (assertEqual "a{b" (Nothing) (matchSingleChars "a{b" 5))

testMatchSingleChars29 = TestCase (assertEqual "a}b" (Nothing) (matchSingleChars "a}b" 0))
testMatchSingleChars30 = TestCase (assertEqual "a}b" 
  (Just Token { tokenType=RightCurlyToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a}b" 1))
testMatchSingleChars31 = TestCase (assertEqual "a}b" (Nothing) (matchSingleChars "a}b" 2))
testMatchSingleChars32 = TestCase (assertEqual "a}b" (Nothing) (matchSingleChars "a}b" 5))

testMatchSingleChars33 = TestCase (assertEqual "a=b" (Nothing) (matchSingleChars "a=b" 0))
testMatchSingleChars34 = TestCase (assertEqual "a=b" 
  (Just Token { tokenType=AssignmentToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a=b" 1))
testMatchSingleChars35 = TestCase (assertEqual "a=b" (Nothing) (matchSingleChars "a=b" 2))
testMatchSingleChars36 = TestCase (assertEqual "a=b" (Nothing) (matchSingleChars "a=b" 5))

testMatchSingleChars37 = TestCase (assertEqual "a,b" (Nothing) (matchSingleChars "a,b" 0))
testMatchSingleChars38 = TestCase (assertEqual "a,b" 
  (Just Token { tokenType=CommaToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a,b" 1))
testMatchSingleChars39 = TestCase (assertEqual "a,b" (Nothing) (matchSingleChars "a,b" 2))
testMatchSingleChars40 = TestCase (assertEqual "a,b" (Nothing) (matchSingleChars "a,b" 5))

testMatchSingleChars41 = TestCase (assertEqual "a^b" (Nothing) (matchSingleChars "a^b" 0))
testMatchSingleChars42 = TestCase (assertEqual "a^b" 
  (Just Token { tokenType=PowerToken, tokenLength=1, 
                tokenPosition=1 })   (matchSingleChars "a^b" 1))
testMatchSingleChars43 = TestCase (assertEqual "a^b" (Nothing) (matchSingleChars "a^b" 2))
testMatchSingleChars44 = TestCase (assertEqual "a^b" (Nothing) (matchSingleChars "a^b" 5))

testMatchNumber1 = TestCase (assertEqual "123" 
   (Just Token { tokenType=NumberToken, tokenLength=3, 
                tokenPosition=0 })   (matchNumber "123" 0))
testMatchNumber2 = TestCase (assertEqual "a123" (Nothing) (matchNumber "a123" 0))
testMatchNumber3 = TestCase (assertEqual "123a" (Nothing) (matchNumber "123a" 0))
testMatchNumber4 = TestCase (assertEqual "12.43" 
   (Just Token { tokenType=NumberToken, tokenLength=5, 
                tokenPosition=0 })   (matchNumber "12.43" 0))
testMatchNumber5 = TestCase (assertEqual "12.43" 
   (Just Token { tokenType=NumberToken, tokenLength=4, 
                tokenPosition=1 })   (matchNumber "12.43" 1))
testMatchNumber6 = TestCase (assertEqual "12.43" (Nothing) (matchNumber "12.43" 2))
testMatchNumber7 = TestCase (assertEqual ".2" (Nothing) (matchNumber ".2" 0))
testMatchNumber8 = TestCase (assertEqual ".2" 
   (Just Token { tokenType=NumberToken, tokenLength=1, 
                tokenPosition=1 })   (matchNumber ".2" 1))
testMatchNumber9 = TestCase (assertEqual ".2a" (Nothing) (matchNumber ".2a" 1))
testMatchNumber10 = TestCase (assertEqual "00.2" 
   (Just Token { tokenType=NumberToken, tokenLength=3, 
                tokenPosition=1 })   (matchNumber "00.2" 1))
testMatchNumber11 = TestCase (assertEqual "00.2" (Nothing) (matchNumber "00.2" 0))
testMatchNumber12 = TestCase (assertEqual "0." 
   (Just Token { tokenType=NumberToken, tokenLength=1, 
                tokenPosition=0 })   (matchNumber "0." 0))
testMatchNumber13 = TestCase (assertEqual "." (Nothing) (matchNumber "." 0))
testMatchNumber14 = TestCase (assertEqual "12.43." 
   (Just Token { tokenType=NumberToken, tokenLength=5, 
                tokenPosition=0 })   (matchNumber "12.43." 0))

testMatchEndToken1 = TestCase (assertEqual "." (Nothing) (matchEndToken "." 0))
testMatchEndToken2 = TestCase (assertEqual "." (Nothing) (matchEndToken "1" 0))
testMatchEndToken3 = TestCase (assertEqual "." (Nothing) (matchEndToken "a" 0))
testMatchEndToken4 = TestCase (assertEqual "12.3" 
   (Just Token { tokenType=EndToken, tokenLength=0, 
                tokenPosition=4 })   (matchEndToken "12.3" 4))
testMatchEndToken5 = TestCase (assertEqual "12.3" 
   (Just Token { tokenType=EndToken, tokenLength=0, 
                tokenPosition=6 })   (matchEndToken "12.3" 6))
testMatchEndToken6 = TestCase (assertEqual "" 
   (Just Token { tokenType=EndToken, tokenLength=0, 
                tokenPosition=0 })   (matchEndToken "" 0))

tests = hUnitTestToTests $ TestList [
                    TestLabel "matchIdentifier Test1"    testMatchIdentifier1,
                    TestLabel "matchIdentifier Test2"    testMatchIdentifier2,
                    TestLabel "matchIdentifier Test3"    testMatchIdentifier3,
                    TestLabel "matchIdentifier Test4"    testMatchIdentifier4,
                    TestLabel "matchIdentifier Test5"    testMatchIdentifier5,
                    TestLabel "matchIdentifier Test6"    testMatchIdentifier6,
                    TestLabel "matchIdentifier Test7"    testMatchIdentifier7,
                    TestLabel "matchIdentifier Test8"    testMatchIdentifier8,
                    TestLabel "matchIdentifier Test9"    testMatchIdentifier9,
                    TestLabel "matchIdentifier Test10"   testMatchIdentifier10,
                    TestLabel "matchIdentifier Test11"   testMatchIdentifier11,
                    TestLabel "matchIdentifier Test12"   testMatchIdentifier12,
                    TestLabel "matchIdentifier Test13"   testMatchIdentifier13,
                    TestLabel "matchIdentifier Test14"   testMatchIdentifier14,
                    TestLabel "matchIdentifier Test15"   testMatchIdentifier15,
                    TestLabel "matchIdentifier Test16"   testMatchIdentifier16,
                    TestLabel "matchIdentifier Test17"   testMatchIdentifier17,
                    TestLabel "matchIdentifier Test18"   testMatchIdentifier18,
                    TestLabel "matchIdentifier Test19"   testMatchIdentifier19,
                    TestLabel "matchIdentifier Test20"   testMatchIdentifier20,
                    TestLabel "matchIdentifier Test21"   testMatchIdentifier21,
                    TestLabel "matchIdentifier Test22"   testMatchIdentifier22,
                    TestLabel "matchIdentifier Test23"   testMatchIdentifier23,
                    TestLabel "matchIdentifier Test24"   testMatchIdentifier24,
                    TestLabel "matchIdentifier Test25"   testMatchIdentifier25,
                    TestLabel "matchIdentifier Test26"   testMatchIdentifier26,
                    TestLabel "matchIdentifier Test27"   testMatchIdentifier27,
                    TestLabel "matchIdentifier Test28"   testMatchIdentifier28,
                    TestLabel "matchIdentifier Test29"   testMatchIdentifier29,
                    TestLabel "matchSingleChars Test1"   testMatchSingleChars1,
                    TestLabel "matchSingleChars Test2"   testMatchSingleChars2,
                    TestLabel "matchSingleChars Test3"   testMatchSingleChars3,
                    TestLabel "matchSingleChars Test4"   testMatchSingleChars4,
                    TestLabel "matchSingleChars Test5"   testMatchSingleChars5,
                    TestLabel "matchSingleChars Test6"   testMatchSingleChars6,
                    TestLabel "matchSingleChars Test7"   testMatchSingleChars7,
                    TestLabel "matchSingleChars Test8"   testMatchSingleChars8,
                    TestLabel "matchSingleChars Test9"   testMatchSingleChars9,
                    TestLabel "matchSingleChars Test10"  testMatchSingleChars10,
                    TestLabel "matchSingleChars Test11"  testMatchSingleChars11,
                    TestLabel "matchSingleChars Test12"  testMatchSingleChars12,
                    TestLabel "matchSingleChars Test13"  testMatchSingleChars13,
                    TestLabel "matchSingleChars Test14"  testMatchSingleChars14,
                    TestLabel "matchSingleChars Test15"  testMatchSingleChars15,
                    TestLabel "matchSingleChars Test16"  testMatchSingleChars16,
                    TestLabel "matchSingleChars Test17"  testMatchSingleChars17,
                    TestLabel "matchSingleChars Test18"  testMatchSingleChars18,
                    TestLabel "matchSingleChars Test19"  testMatchSingleChars19,
                    TestLabel "matchSingleChars Test20"  testMatchSingleChars20,
                    TestLabel "matchSingleChars Test21"  testMatchSingleChars21,
                    TestLabel "matchSingleChars Test22"  testMatchSingleChars22,
                    TestLabel "matchSingleChars Test23"  testMatchSingleChars23,
                    TestLabel "matchSingleChars Test24"  testMatchSingleChars24,
                    TestLabel "matchSingleChars Test25"  testMatchSingleChars25,
                    TestLabel "matchSingleChars Test26"  testMatchSingleChars26,
                    TestLabel "matchSingleChars Test27"  testMatchSingleChars27,
                    TestLabel "matchSingleChars Test28"  testMatchSingleChars28,
                    TestLabel "matchSingleChars Test29"  testMatchSingleChars29,
                    TestLabel "matchSingleChars Test30"  testMatchSingleChars30,
                    TestLabel "matchSingleChars Test31"  testMatchSingleChars31,
                    TestLabel "matchSingleChars Test32"  testMatchSingleChars32,
                    TestLabel "matchSingleChars Test33"  testMatchSingleChars33,
                    TestLabel "matchSingleChars Test34"  testMatchSingleChars34,
                    TestLabel "matchSingleChars Test35"  testMatchSingleChars35,
                    TestLabel "matchSingleChars Test36"  testMatchSingleChars36,
                    TestLabel "matchSingleChars Test37"  testMatchSingleChars37,
                    TestLabel "matchSingleChars Test38"  testMatchSingleChars38,
                    TestLabel "matchSingleChars Test39"  testMatchSingleChars39,
                    TestLabel "matchSingleChars Test40"  testMatchSingleChars40,
                    TestLabel "matchSingleChars Test41"  testMatchSingleChars41,
                    TestLabel "matchSingleChars Test42"  testMatchSingleChars42,
                    TestLabel "matchSingleChars Test43"  testMatchSingleChars43,
                    TestLabel "matchSingleChars Test44"  testMatchSingleChars44,
                    TestLabel "testMatchNumber Test01"  testMatchNumber1,
                    TestLabel "testMatchNumber Test02"  testMatchNumber2,
                    TestLabel "testMatchNumber Test03"  testMatchNumber3,
                    TestLabel "testMatchNumber Test04"  testMatchNumber4,
                    TestLabel "testMatchNumber Test05"  testMatchNumber5,
                    TestLabel "testMatchNumber Test06"  testMatchNumber6,
                    TestLabel "testMatchNumber Test07"  testMatchNumber7,
                    TestLabel "testMatchNumber Test08"  testMatchNumber8,
                    TestLabel "testMatchNumber Test09"  testMatchNumber9,
                    TestLabel "testMatchNumber Test10"  testMatchNumber10,
                    TestLabel "testMatchNumber Test11"  testMatchNumber11,
                    TestLabel "testMatchNumber Test12"  testMatchNumber12,
                    TestLabel "testMatchNumber Test13"  testMatchNumber13,
                    TestLabel "testMatchNumber Test14"  testMatchNumber14,
                    TestLabel "testMatchEndToken Test01"  testMatchEndToken1,
                    TestLabel "testMatchEndToken Test02"  testMatchEndToken2,
                    TestLabel "testMatchEndToken Test03"  testMatchEndToken3,
                    TestLabel "testMatchEndToken Test04"  testMatchEndToken4,
                    TestLabel "testMatchEndToken Test05"  testMatchEndToken5,
                    TestLabel "testMatchEndToken Test06"  testMatchEndToken6
                 ]

main = defaultMain tests 
