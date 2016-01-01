{-# LANGUAGE Haskell2010 #-} 
{-# LANGUAGE Safe #-}

module Hmcalc.Lexer(
    TokenType(..), 
    Token(..), 
    LexerError(..),
    LexerResult,
    LexerState,
    matchIdentifier,
    matchNumber,
    matchSingleChars,
    matchEndToken,
    initializeLexer
  ) where

import Data.Char (isLetter, isAlphaNum, isDigit, isSpace)
import Control.Monad.State
import Data.Maybe (fromJust, isJust, isNothing)
import Data.List (find)

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
                | PowerToken        -- ^ '^' token
                | TransposeToken    -- ^ `'` token
                deriving (Show, Eq) 

-- | A 'Token' containes all information about a token, i.e. its literal value, the position in the input string 
--   and its TokenType
data Token = Token { tokenType       :: TokenType -- ^ type of the token
                     , tokenLength   :: Int       -- ^ length of the token 
                     , tokenPosition :: Int       -- ^ position in the input string
                   } deriving (Show, Eq)

-- | The 'LexerError' indicates a failure in the lexing process
data LexerError =  InvalidToken { invalidTokenPosition :: Int }  -- ^ Token is invalid, e.g. contains invalid characters 

-- | The 'LexerInternalState' contains the input string and the current position in the input string 
data LexerInternalState = LexerInternalState { lexerString :: String   -- ^ Input string
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
 initialState _ = (LexerResult ( Right Token { tokenType=StartToken, tokenLength = 0, tokenPosition = 0 } ),  
   LexerInternalState { lexerString=string, lexerPosition=0 } )  

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
                                                          lfun len = Just Token { tokenType=ttype, tokenLength=len, 
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
                                               ret t = return Token { tokenType=t, tokenLength=1, tokenPosition=pos }  
                                           in case start of 
                                             '+'  -> ret PlusToken
                                             '-'  -> ret MinusToken
                                             '*'  -> ret AsteriskToken
                                             '/'  -> ret DivisionToken
                                             '('  -> ret LeftBracketToken
                                             ')'  -> ret RightBracketToken
                                             '{'  -> ret LeftCurlyToken
                                             '}'  -> ret RightCurlyToken
                                             ','  -> ret CommaToken
                                             '='  -> ret AssignmentToken
                                             '^'  -> ret PowerToken
                                             '\'' -> ret TransposeToken 
                                             _    -> Nothing

-- | The 'checkNumberStart' function checks, if a given combination of digits forms a number
--   For example: '0012' is not a valid number, but '0.12' is
checkNumberStart :: String  -- ^ input string 
                    -> Bool -- ^ is start of valid number
checkNumberStart s | null s        = False
                   | length s == 1 = isDigit $ head s
                   | otherwise     = isDigit (head s) && not (( head s == '0') && isDigit (head $ tail s)) 

-- | Variant of 'takeWhile' which takes as many elements from a list as boolfunc1 || boolfunc2 is true.
--   But: the second boolean function may only be true 1 time, and also not at the end of the list
takeWhileNoSecond :: ( a -> Bool )        -- ^ First boolean function, may be true any number of times
                     -> ( a -> Bool )     -- ^ Second boolean function, may only be true once 
                     -> [a]               -- ^ Input list 
                     -> [a]               -- ^ Output list
takeWhileNoSecond _  _  []  = []
takeWhileNoSecond f1 f2 lst | f2 (head lst)  = [] 
                            | otherwise      = let before = takeWhile f1 lst
                                                   rest   = drop (length before) lst
                                                   after  = if (length rest > 1) && f2 (head rest) 
                                                            then head rest : takeWhile f1 (tail rest)
                                                            else []
                                               in if null after || f2 (last after) then before else before ++ after 

-- | The 'matchNumber' function matches a single number, e.g. '12.43'
matchNumber :: String         -- ^ input string 
               -> Int         -- ^ position in input string 
               -> Maybe Token -- ^ 'Nothing' if no number has been found at the given position, else the token 
matchNumber s pos = positionMatchHelper NumberToken s pos 
                      (\string -> if not $ checkNumberStart string then Nothing 
                                  else let len = length $ takeWhileNoSecond isDigit (== '.') string
                                       in if length string == len || not (isAlphaNum $ head $ drop len string)
                                          then return len 
                                          else Nothing
                      ) 
-- | The 'matchEndToken' function matches the end of the input stream
matchEndToken :: String     -- ^ input string 
                 -> Int     -- ^ position in input string
                 -> Maybe Token -- ^ Nothing of the position is a valid position in the input string, else the EndToken
matchEndToken s pos | pos >= length s = return Token { tokenType=EndToken, tokenLength=0, tokenPosition=pos } 
                    | otherwise       = Nothing


-- | The 'matchFunctions' function returns a list of all token matcher functions
matchFunctions :: [String -> Int -> Maybe Token]
matchFunctions = [matchIdentifier, matchSingleChars, matchNumber, matchEndToken]

-- | Increase the position inside the 'LexerInternalState' by the given length
increasePosition :: LexerInternalState         -- ^ Internal lexer state
                    -> Int                     -- ^ Increase counter
                    -> LexerInternalState      -- ^ Resulting state
increasePosition st ln = LexerInternalState { lexerString = lexerString st,
                                              lexerPosition = lexerPosition st + ln } 

-- | Skips all whitespace at the current lexer state's position and returns the new state
skipWhitespace :: LexerInternalState -> LexerInternalState
skipWhitespace state @ LexerInternalState {lexerString=s, lexerPosition=p}
                  = let work = drop p s
                        ws   = length (takeWhile isSpace work)
                    in increasePosition state ws 

-- | Get the next token  
getNextToken :: LexerResult -> LexerState
getNextToken (LexerResult (Left error))  = do oldState <- get
                                              put oldState
                                              return (LexerResult (Left error))
getNextToken (LexerResult (Right _ ))    = do oldState <- get 
                                              let wsState = skipWhitespace oldState
                                                  match = find isJust (map (($ (lexerString wsState, 
                                                                                lexerPosition wsState)) . uncurry) 
                                                                       matchFunctions)   
                                              if isNothing match then do put oldState
                                                                         return $ LexerResult (Left (InvalidToken 
                                                                                               (lexerPosition wsState))) 
                                              else let token = fromJust $ fromJust match 
                                                   in do put (increasePosition wsState (tokenLength token)) 
                                                         return $ LexerResult (Right token) 
