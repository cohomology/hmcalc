module Lexer(
    TokenType(..), 
    Token(..), 
    LexerError(..),
    LexerResult,
    LexerState,
    matchIdentifier,
    matchNumber,
    matchSingleChars
  ) where

import Prelude hiding ((&&), (||), not) 
import Data.Char (isLetter, isSpace, isAlphaNum, isDigit)
import Test.HUnit (Test(..), assertEqual, runTestTT) 
import Control.Monad.State
import Data.Boolean ((&&), (||), not)

-- | The 'TokenType' describes the different kinds of tokens
data TokenType =  StartToken        -- ^ Artificial token, corresponding to a lexer state 
                                    --   where no token has been lexed 
                | EndToken          -- ^ Artificial token, indicating that the lexer has 
                                    --   reached the input string 
                | IdentifierToken   -- ^ Identifier, i.e. string starting with a letter and 
                                    --   containing arbitrary alphanumeric chararacters afterwards 
                | NumberToken       -- ^ Rational number, i.e. '12' or '12.34' without sign. Tokens 
                                    --   like '.5' or '12.' are not allowed 
                | PlusToken         -- ^ '+' token 
                | MinusToken        -- ^ '-' token 
                | AsteriskToken     -- ^ '*' token 
                | DivisionToken     -- ^ '/' token 
                | LeftBracketToken  -- ^ '(' token 
                | RightBracketToken -- ^ ')' token 
                | LeftCurlyToken    -- ^ '{' token  
                | RightCurlyToken   -- ^ '}' token 
                | AssignmentToken   -- ^ '=' token 
                | CommaToken        -- ^ ',' token
                deriving (Show, Eq) 

-- | A 'Token' containes all information about a token, i.e. its literal value, the position in the input string 
--   and its TokenType
data Token = Token { tokenType       :: TokenType -- ^ type of the token
                     , tokenLength   :: Int       -- ^ length of the token 
                     , tokenPosition :: Int       -- ^ position in the input string
                   } deriving (Show, Eq)

-- | The 'LexerError' indicates a failure in the lexing process
data LexerError =  InvalidToken { failurePosition :: Int }  -- ^ Token is invalid, e.g. contains invalid characters 

-- | The 'LexerInternalState' contains the input string and the current position in the input string 
data LexerInternalState = LexerPosition { lexerString :: String   -- ^ Input string
                                          , lexerPosition :: Int  -- ^ Position in input string 
                                        }
-- | The 'LexerResult' is the result of an invokation of 'lookAhead' or 'getNextToken'
newtype LexerResult = LexerResult ( Either LexerError Token ) 

-- | The 'LexerState' contains the result of an invokation of 'lookAhead' or 'getNextToken', i.e. the 
--   lexer state and the obtained token.
type LexerState = State LexerInternalState LexerResult 

-- | The 'initializeLexer' function initializes the state of the Lexer
initializeLexer :: String -> LexerState
initializeLexer string = state initialState where
 initialState :: LexerInternalState -> (LexerResult, LexerInternalState)
 initialState state = (LexerResult ( Right ( Token { tokenType=StartToken, tokenLength = 0, tokenPosition = 0 } ) ),  
   LexerPosition { lexerString=string, lexerPosition=0 } )  

-- | Helper function for the different 'match' routines. It removes as many characters from the string constituting 
--   the first parameter as the second parameter suggests and invokes the given function (i.e. the third parameter) 
--   on the resulting string. It returns 'Nothing' if the given function returns 'Nothing' and else in constructs a 
--   ktoken from the overall information provided.
positionMatchHelper :: TokenType 
                       -> String                      -- ^ input string 
                       -> Int                         -- ^ position in string
                       -> ( String -> Maybe Int )     -- ^ function to invoke, function should return length of token  
                       -> Maybe Token                 -- ^ final token
positionMatchHelper ttype s pos f | length s <= pos = Nothing
                                  | otherwise       = let tlength = f $ drop pos s 
                                                          lfun len = Just $ Token { tokenType=ttype, tokenLength=len, 
                                                                                    tokenPosition=pos } 
                                                      in maybe Nothing lfun tlength 

-- | The 'matchIdentifier' function matches a single identifier.
matchIdentifier :: String              -- ^ input string 
                   -> Int              -- ^ position in input string 
                   -> Maybe Token      -- ^ Nothing if no identifier matched, else the generated token 
matchIdentifier s pos = positionMatchHelper IdentifierToken s pos 
                          ( \string -> if isLetter $ head string then return $ length $ takeWhile isAlphaNum string 
                                       else Nothing 
                          ) 

-- | The 'matchSingleChars' function matches all operators and brackets 
matchSingleChars :: String      -- ^ input string 
                 -> Int         -- ^ position in input string 
                 -> Maybe Token -- ^ 'Nothing', if no operator or bracket has been matched, else the generated token 
matchSingleChars s pos | length s <= pos = Nothing
                       | otherwise       = let start = head $ drop pos s  
                                               ret s = return Token { tokenType=s, tokenLength=1, tokenPosition=pos }  
                                           in case start of 
                                             '+' -> ret PlusToken
                                             '-' -> ret MinusToken
                                             '*' -> ret AsteriskToken
                                             '/' -> ret DivisionToken
                                             '(' -> ret LeftBracketToken
                                             ')' -> ret RightBracketToken
                                             '{' -> ret LeftCurlyToken
                                             '}' -> ret RightCurlyToken
                                             ',' -> ret CommaToken
                                             _   -> Nothing

-- | The 'checkNumberStart' function checks, if a given combination of digits forms a number
--   For example: '0012' is not a valid number, but '0.12' is
checkNumberStart :: String  -- ^ input string 
                    -> Bool -- ^ is start of valid number
checkNumberStart s | length s == 1 = isDigit $ head s
                   | otherwise     = (isDigit $ head s) && (not ( ( head s == '0') && (isDigit $ head $ tail s ))) 

-- | Counts the number of digits before the second instance of '.'. Assumes that the string does not start with '.'
--   and consists of digits and points only. It also doesn't count the last instance of '.', if the final string 
--   would end with '.'.
countBeforeSecondInstanceOf :: String  -- ^ Input string
                               -> Int  -- ^ Number of characters before second '.'.
countBeforeSecondInstanceOf s = let cutDo :: String -> Bool -> String
                                    cutDo []     _     = [] 
                                    cutDo string True  = let h = head string 
                                                         in if h /= '.' then [h] ++ cutDo (tail string) True else [] 
                                    cutDo string False = let h = head string 
                                                         in if h /= '.' then [h] ++ cutDo (tail string) False
                                                            else [h] ++ cutDo (tail string) True 
                                    cuttedStr = cutDo s False
                                in if last cuttedStr == '.' then -1 + length cuttedStr else length cuttedStr

-- | The 'matchNumber' function matches a single number, e.g. '12.43'
matchNumber :: String         -- ^ input string 
               -> Int         -- ^ position in input string 
               -> Maybe Token -- ^ 'Nothing' if no number has been found at the given position, else the token 
matchNumber s pos = positionMatchHelper NumberToken s pos 
                      (\string -> if not $ checkNumberStart string then Nothing 
                                  else let isPoint c = c == '.' 
                                           lst = takeWhile ( isDigit || isPoint ) string 
                                       in return $ countBeforeSecondInstanceOf lst                      ) 


