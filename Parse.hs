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
          return $ EVar v

-- | Parses a constructor Pack expression: Pack{n,m}
ePack :: Parser Expr
ePack = do _ <- symbol "Pack"
           _ <- symbol "{"
           tag <- int
           _ <- symbol ","
           arity <- int
           _ <- symbol "}"
           return $ EPack tag arity

-- | Parses a parenthesized expression
eParen :: Parser Expr
eParen = do _ <- symbol "("
            e <- expr
            _ <- symbol ")"
            return e

-- | Parses an atomic expression (variable, number, Pack, or parenthesized)
aexpr :: Parser Expr
aexpr = try ePack <|> try eInt <|> try eVar <|> eParen

--- ### Operator Table for Binary Expressions

-- | Table of binary operators with precedence and associativity
binOps :: [[Ex.Operator String () Identity Expr]]
binOps =
  [ [ Ex.Infix (return EAp) Ex.AssocLeft ]  -- Function application
  , [ infixOp "*" Ex.AssocRight ]
  , [ infixOp "/" Ex.AssocNone ]
  , [ infixOp "+" Ex.AssocRight ]
  , [ infixOp "-" Ex.AssocNone ]
  , [ infixOp "==" Ex.AssocNone
    , infixOp "~=" Ex.AssocNone
    , infixOp ">"  Ex.AssocNone
    , infixOp ">=" Ex.AssocNone
    , infixOp "<"  Ex.AssocNone
    , infixOp "<=" Ex.AssocNone
    ]
  , [ infixOp "&" Ex.AssocRight ]
  , [ infixOp "|" Ex.AssocRight ]
  ]
  where
    infixOp name assoc =
      Ex.Infix (do { _ <- symbol name
                   ; return (\x y -> EAp (EAp (EVar name) x) y) }) assoc

--- ### Expressions

-- | Parses expressions with correct precedence and associativity
expr :: Parser Expr
expr = Ex.buildExpressionParser binOps aexpr

--- ### Top-Level Declarations

-- | Parses a single supercombinator declaration
decl :: Parser Decl
decl = do name <- id
          params <- many id
          _ <- symbol "="
          body <- expr
          return (name, params, body)

-- | Parses a complete Core program (one or more declarations)
core :: Parser Core
core = do decls <- decl `sepBy` (symbol ";")
          return $ M.fromList [(n, v) | v@(n, _, _) <- decls]

-- | Entry point for parsing a Core program
parseCore :: String -> Either ParseError Core
parseCore text = parse core "Core" text
