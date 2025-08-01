-- | The parser goes here

module Parse where

import Prelude hiding (identifier)
import Types
import qualified Data.HashMap.Lazy as M

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Data.Functor.Identity (Identity)

--- The Parser
--- ----------

-- | Parses a specific symbol and consumes trailing spaces
symbol :: String -> Parser String
symbol s = string s <* spaces

-- | Parses a positive integer and consumes trailing spaces
int :: Parser Int
int = do
  digits <- many1 digit <?> "an integer"
  spaces
  return (read digits)

-- | Parses a lowercase identifier and consumes trailing spaces
identifier :: Parser Name
identifier = do
  first <- lower <?> "identifier (must start lowercase)"
  rest  <- many (alphaNum <|> oneOf "'_")
  let name = first : rest
  spaces
  if name `elem` reservedWords
    then unexpected $ "reserved word, next time Gadget... " ++ show name
    else return name
  
-- / Reserved word protection so it doesn't get parsed by the identifier
reservedWords :: [String]
reservedWords = ["let", "letrec", "in", "case", "of", "Pack", "->"]

--- ### Atomic Expressions

eInt :: Parser Expr
eInt = ENum <$> int

eVar :: Parser Expr
eVar = EVar <$> identifier

ePack :: Parser Expr
ePack = do
  _ <- symbol "Pack"
  _ <- symbol "{"
  tag <- int
  _ <- symbol ","
  arity <- int
  _ <- symbol "}"
  return $ EPack tag arity

eParen :: Parser Expr
eParen = between (symbol "(") (symbol ")") expr

aexpr :: Parser Expr
aexpr = try ePack <|> try eInt <|> try eVar <|> eParen

-- | Parses let and letrec expressions
eLet :: Parser Expr
eLet = do
  _ <- symbol "let"
  defs <- defn `sepBy1` symbol ";"
  _ <- symbol "in"
  body <- expr
  return $ ELet False defs body
  
eLetRec :: Parser Expr
eLetRec = do
  _ <- symbol "letrec"
  defs <- defn `sepBy1` symbol ";"
  _ <- symbol "in"
  body <- expr
  return $ ELet True defs body

-- | Parses a single definition: x = expr
defn :: Parser (Name, Expr)
defn = do
  name <- identifier
  _ <- symbol "="
  val <- expr
  return (name, val)

-- | Parses case expressions
eCase :: Parser Expr
eCase = do
  _ <- symbol "case"
  scrutinee <- expr
  _ <- symbol "of"
  alts <- alt `sepBy1` symbol ";"
  return $ ECase scrutinee alts

-- | Parses a case alternative: <tag> x y -> expr
alt :: Parser (Int, [Name], Expr)
alt = do
  _ <- symbol "<"
  tag <- int
  _ <- symbol ">"
  vars <- many identifier
  _ <- symbol "->"
  rhs <- expr
  return (tag, vars, rhs)

-- | Parses a lambda expression: \x y . expr
eLam :: Parser Expr
eLam = do
  _ <- symbol "\\"
  params <- many1 identifier
  _ <- symbol "."
  body <- expr
  return $ ELam params body

--- ### Operator Table

binOps :: [[Ex.Operator String () Identity Expr]]
binOps =
  [ 
    [op "*" Ex.AssocLeft]
  , [op "/" Ex.AssocNone]
  , [op "+" Ex.AssocRight]
  , [op "-" Ex.AssocNone]
  , [op "==" Ex.AssocNone, op "~=" Ex.AssocNone, op ">" Ex.AssocNone, op ">=" Ex.AssocNone, op "<" Ex.AssocNone, op "<=" Ex.AssocNone]
  , [op "&" Ex.AssocRight]
  , [op "|" Ex.AssocRight]
  ]
  where
    op s assoc = Ex.Infix (symbol s >> return (\x y -> EAp (EAp (EVar s) x) y)) assoc

--- ### Expressions

-- simple units: letrec, let, case, lambda, atomic, but not so simple concepts
simple :: Parser Expr
simple = try eLetRec <|> try eLet <|> try eCase <|> try eLam <|> aexpr

-- application by juxtaposition, left-associative
application :: Parser Expr
application = do
  parts <- many1 simple
  return $ foldl1 EAp parts

-- full expression: infix operators applied over application
expr :: Parser Expr
expr = Ex.buildExpressionParser binOps application

--- ### Top-Level Declarations

decl :: Parser Decl
decl = do
  name <- identifier
  params <- many identifier
  _ <- symbol "="
  body <- expr
  return (name, params, body)

core :: Parser Core
core = do
  decls <- decl `sepBy1` symbol ";"
  eof
  return $ M.fromList [(n, d) | d@(n, _, _) <- decls]

parseCore :: String -> Either ParseError Core
parseCore = parse core "Core"
