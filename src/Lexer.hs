module Lexer where

import Data.Ratio
import Data.Word
import Data.Char (isLetter, isSpace, isAlphaNum, isDigit)
import Control.Monad.State

data TokenType = StartToken | EndToken | IdentifierToken String | NumberToken Rational | PlusToken | MinusToken | AsteriskToken | DivisionToken |
                 LeftBracketToken | RightBracketToken | LeftCurlyToken | RightCurlyToken deriving (Show, Eq) 
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

matchIdentifier :: String -> Maybe (TokenType, Int)
matchIdentifier [] = Nothing 
matchIdentifier s | isLetter $ head s = let token = takeWhile isAlphaNum s 
                                        in Just $ (IdentifierToken token, length token)
                  | otherwise         = Nothing 
                  
matchSingleChars :: String -> Maybe (TokenType, Int)
matchSingleChars s = let ret s = return (s, 1) 
                     in case head s of 
                     '+' -> ret PlusToken
                     '-' -> ret MinusToken
                     '*' -> ret AsteriskToken
                     '/' -> ret DivisionToken
                     '(' -> ret LeftBracketToken
                     ')' -> ret RightBracketToken
                     '{' -> ret LeftCurlyToken
                     '}' -> ret RightCurlyToken
                     _   -> Nothing

checkNumberStart :: String -> Bool
checkNumberStart [] = False
checkNumberStart s | length s == 1  = isDigit $ head s
                   | otherwise      = (isDigit $ head s) && (not ( ( head s == '0') && (isDigit $ head $ tail s ))) 

matchNumber :: String -> Maybe (TokenType, Int)
matchNumber []  = Nothing
matchNumber s | checkNumberStart s =  let matchNumberDo :: String -> (String, String)
                                          matchNumberDo [] = ([],[])
                                          matchNumberDo s  = let beforeDot    = takeWhile isDigit s
                                                                 rest         = drop (length beforeDot) s
                                                                 afterDot     = if length rest > 1 && head rest == '.' 
                                                                                then takeWhile isDigit (tail rest) else []  
                                                             in (beforeDot, afterDot)
                                          (bdot, adot) = matchNumberDo s 
                                          bdotInt      = read ( bdot ++ adot ) :: Integer
                                          completeLn   = ( length bdot ) + if length adot == 0 then 0 else 1 + length adot  
                                      in Just ( NumberToken $ bdotInt % ( 10 ^ (length adot) ), completeLn)  
              | otherwise         = Nothing

{- 
lookAhead :: Word -> LexerState
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
