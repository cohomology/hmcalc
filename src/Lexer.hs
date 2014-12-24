module Lexer where

import Data.Ratio
import Data.Char (isLetter, isSpace, isAlphaNum, isDigit)
import Control.Monad.State

data TokenType = StartToken | EndToken | IdentifierToken String | NumberToken Rational | PlusToken | MinusToken | AsteriskToken | DivisionToken |
                 LeftBracketToken | RightBracketToken | LeftCurlyToken | RightCurlyToken 
data Token = Token { tokenType     :: TokenType,
                     tokenPosition :: Int }
data LexerError = InvalidState
data LexerPosition = LexerPosition { lexerString :: String, lexerPosition :: Int, lookAhead :: [Token] }
newtype LexerResult = LexerResult ( Either LexerError Token ) 
type LexerState = State LexerPosition LexerResult 

initializeLexer :: String -> LexerState
initializeLexer string = state initialState where
 initialState :: LexerPosition -> (LexerResult, LexerPosition)
 initialState state = (LexerResult ( Right ( Token { tokenType = StartToken, tokenPosition = 0 } ) ),  
   LexerPosition { lexerString=string, lexerPosition=0, lookAhead = [] } )  

matchIdentifier :: String -> String
matchIdentifier s | isAlphaNum $ head s = head s : matchIdentifier (tail s) 
                  | otherwise           = []

charToTokenType :: Char -> Maybe TokenType
charToTokenType c = case c of 
  '+' -> Just PlusToken
  '-' -> Just MinusToken
  '*' -> Just AsteriskToken
  '/' -> Just DivisionToken
  '(' -> Just LeftBracketToken
  ')' -> Just RightBracketToken
  '{' -> Just LeftCurlyToken
  '}' -> Just RightCurlyToken
  _   -> Nothing 
                         
matchNumber :: String -> (String, Rational)
matchNumber s = let matchNumberDo :: String -> Bool -> (String, String)
                    matchNumberDo s True  | isDigit $ head s = let (beforeDot, afterDot) = matchNumberDo (tail s) True 
                                                               in (head s : beforeDot, afterDot)
                                          | head s == '.'    = matchNumberDo (tail s) False
                                          | otherwise        = ([], [])  
                    matchNumberDo s False | isDigit $ head s = let (beforeDot, afterDot) = matchNumberDo (tail s) False 
                                                               in (beforeDot, head s : afterDot)
                                          | otherwise        = ([], [])  
                    (bdot, adot) = matchNumberDo s True  
                    bdotInt      = read ( bdot ++ adot ) :: Integer
                    complete     = bdot ++ if length adot == 0 then [] else ['.'] ++ adot  
                in (complete, bdotInt % ( 10 ^ (length adot) ) ) 

{- 
lookAhead :: Integer -> LexerState
lookAhead la = state lookAheadState where
  lookAheadState :: LexerPosition -> (LexerResult, LexerPosition)
  lookAheadState lpos@(LexerPosition { lexerString=cstr, lexerPosition=cpos, lookAhead=cla } )
    | length cla >= la       = cla !! la
    | length str >= pos      = ( LexerResult $ Right $ Token { tokenType = EOF, tokenPosition=pos }, lpos) 
    | isSpace $ cstr !! cpos = lookAheadState $ LexerPosition (tail cstr) (cpos + 1) cla
    | la = 1                 = let lookAheadDo :: String -> Maybe (String, TokenType)
                                   lookAheadDo s | isLetter   $ head s               = let id=matchIdentifier s
                                                                                       in Just (id, IdentifierToken id) 
                                                 | isDigit    $ head s               = let (str, num) = matchNumber s
                                                                                       in Just (str, NumberToken num) 
                                                 | isJust $ charToTokenType $ head s = Just (head s, charToTokenType $ head s)
                                                 | Nothing  
                                   match = lookAheadDo (cla !! la) 
-}
