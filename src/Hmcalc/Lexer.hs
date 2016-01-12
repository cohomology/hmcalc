module Hmcalc.Lexer(
  TokenType(..)
) where

-- | The 'TokenType' describes the different kinds of tokens
data TokenType =  IdentifierToken String  -- ^ Identifier, i.e. string starting with a letter and 
                                          --   containing arbitrary alphanumeric chararacters afterwards 
                | NumberToken String      -- ^ Rational number, i.e. '12' or '12.34' without sign. Tokens 
                                          --   like '.5' or '12.' are not allowed 
                | Whitespaces String      -- ^ A number of whitespace like characters
                | PlusToken               -- ^ '+' token 
                | MinusToken              -- ^ '-' token 
                | AsteriskToken           -- ^ '*' token 
                | DivisionToken           -- ^ '/' token 
                | LeftBracketToken        -- ^ '(' token 
                | RightBracketToken       -- ^ ')' token 
                | LeftCurlyToken          -- ^ '{' token  
                | RightCurlyToken         -- ^ '}' token 
                | AssignmentToken         -- ^ '=' token 
                | CommaToken              -- ^ ',' token
                | PowerToken              -- ^ '^' token
                | TransposeToken          -- ^ `'` token
                | UnderscoreToken         -- ^ '_' token
                deriving (Show, Eq) 
