-- | The parser goes here

module Parse where

import Prelude hiding (id)

import Types

import qualified Data.HashMap.Lazy as M

import Data.Functor.Identity
import Control.Monad
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import qualified Text.ParserCombinators.Parsec.Expr as Ex

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

-- | Parses a specific symbol and consumes trailing spaces
symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

-- | Parses a positive integer and consumes trailing spaces
int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

-- | Parses a lowercase identifier and consumes trailing spaces
id :: Parser Name
id = do first <- oneOf ['a' .. 'z']
        rest <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_")
        spaces
        return $ first:rest

--- ### Atomic Expressions

-- | Parses an integer expression
eInt :: Parser Expr
eInt = do i <- int
          return $ ENum i

-- | Parses a variable expression
eVar :: Parser Expr
eVar = do v <- id
          return
