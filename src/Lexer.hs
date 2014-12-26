module Lexer where

import Data.Ratio
import Data.Word
import Data.Char (isLetter, isSpace, isAlphaNum, isDigit)
import Control.Monad.State

-- | The 'TokenType' describes the different kinds of tokens
data TokenType =  StartToken        -- ^ Artificial token, corresponding to a lexer state where no token has been lexed 
                | EndToken          -- ^ Artificial token, indicating that the lexer has reached the input string 
                | IdentifierToken   -- ^ Identifier, i.e. string starting with a letter and containing arbitrary alphanumeric chararacters afterwards 
                | NumberToken       -- ^ Rational number, i.e. '12' or '12.34' without sign. Things like '.5' or '12.' are not allowed 
                | PlusToken         -- ^ '+' token 
                | MinusToken        -- ^ '-' token 
                | AsteriskToken     -- ^ '*' token 
                | DivisionToken     -- ^ '/' token 
                | LeftBracketToken  -- '(' token 
                | RightBracketToken -- ')' token 
                | LeftCurlyToken    -- '{' token  
                | RightCurlyToken   -- '}' token 
                | AssignmentToken   -- '=' token 
                deriving (Show, Eq) 

-- | A 'Token' containes all information about a token, i.e. its literal value, the position in the input string and its TokenType
data Token = Token { tokenType     :: TokenType -- ^ type of the token
                     , tokenString   :: String  -- ^ string representing the token
                     , tokenPosition :: Int     -- ^ position in the input string
                   }

-- | The 'LexerError' indicates a failure in the lexing process
data LexerError =   InvalidToken String     -- ^ Token is invalid, e.g. contains invalid characters 

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
 initialState state = (LexerResult ( Right ( Token { tokenType=StartToken, tokenString=[], tokenPosition = 0 } ) ),  
   LexerPosition { lexerString=string, lexerPosition=0 } )  

-- | The 'matchIdentifier' function matches a single identifier.
matchIdentifier :: String              -- ^ input string 
                   -> Int              -- ^ position in input string 
                   -> Maybe Token      -- ^ Nothing if no identifier matched, else the generated token 
matchIdentifier s pos | length s <= pos = Nothing 
                      | otherwise       = let start = drop pos s
                                              token = takeWhile isAlphaNum start
                                          in if ( length token > 0 ) && ( isLetter $ head token ) 
                                             then return Token { tokenType = IdentifierToken, tokenString = token, tokenPosition = pos }
                                             else Nothing

-- | The 'matchSingleChars' function matches all operators and brackets 
matchSingleChars :: String      -- ^ input string 
                 -> Int         -- ^ position in input string 
                 -> Maybe Token -- ^ 'Nothing', if no operator or bracket has been matched, else the generated token 
matchSingleChars s pos | length s <= pos = Nothing
                       | otherwise       = let start = head $ drop pos s  
                                               ret s = return Token { tokenType=s, tokenString=[start], tokenPosition=pos }  
                                           in case start of 
                                             '+' -> ret PlusToken
                                             '-' -> ret MinusToken
                                             '*' -> ret AsteriskToken
                                             '/' -> ret DivisionToken
                                             '(' -> ret LeftBracketToken
                                             ')' -> ret RightBracketToken
                                             '{' -> ret LeftCurlyToken
                                             '}' -> ret RightCurlyToken
                                             _   -> Nothing

-- | The 'checkNumberStart' function checks, if a given combination of digits forms a number
--   For example: '0012' is not a valid number, but '0.12' is
checkNumberStart :: String  -- ^ input string 
                    -> Int  -- ^ position in input string
                    -> Bool -- ^ is start of valid number
checkNumberStart s pos | length s <= pos = False 
                       | otherwise       = let start = drop pos s
                                           in case length start of 
                                              1 -> isDigit $ head start 
                                              _ -> (isDigit $ head start) && (not ( ( head start == '0') && (isDigit $ head $ tail start ))) 

-- | The 'matchNumber' function matches a single number, e.g. '12.43'
matchNumber :: String         -- ^ input string 
               -> Int         -- ^ position in input string 
               -> Maybe Token -- ^ 'Nothing' if no number has been found at the given position, else the token 
matchNumber s pos | length s <= pos        = Nothing 
                  | checkNumberStart s pos = let matchNumberDo :: String -> String 
                                                 matchNumberDo s  = let beforeDot    = takeWhile isDigit s
                                                                        rest         = drop (length beforeDot) s
                                                                        afterDot     = if length rest > 1 && head rest == '.' 
                                                                                       then ['.'] ++ takeWhile isDigit (tail rest) else []  
                                                                    in beforeDot ++ afterDot 
                                                 number = matchNumberDo (drop pos s) 
                                             in return Token { tokenType=NumberToken, tokenString=number, tokenPosition=pos } 
                 | otherwise               = Nothing
