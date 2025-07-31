-- | Here are the types for the Core interpreter.
-- You may add constructors if you wish, but do not modify any that are already here.

module Types where

import qualified Data.HashMap.Lazy as M

type Name = String
type IsRec = Bool

-- | Core language expressions (AST)
data Expr = EVar Name                  -- Variables
          | ENum Int                   -- Numbers
          | EPack Int Int              -- Constructors
          | EAp Expr Expr              -- Applications
          | ELet
               IsRec                   -- is the let recursive?
               [(Name, Expr)]          -- Local Variable definitions
               Expr                    -- Body of the let
          | ECase                      -- Case Expressions
               Expr
               [(Int, [Name], Expr)]   -- Alternatives
          | ELam [Name] Expr           -- Functions (Lambdas)
   deriving (Eq, Show)

-- | A top-level function declaration: name, parameters, and body
type Decl = (Name, [Name], Expr)       -- The name, parameter list, and body of a supercombinator declaration

-- | A Core program is a map of supercombinator names to their declarations
type Core = M.HashMap Name Decl        -- A core program is an environment of declarations

--- ### Runtime evaluation types

-- | Values produced by evaluation
data Value
  = VInt Int                        -- Integer literal
  | VFun (Value -> Eval Value)      -- Curried function (closure)
  | VPack Int [Value]               -- Constructor tag + arguments
  deriving (Eq)                     -- Add Show if you want debug print

-- | Evaluation result monad with error reporting
type Eval a = Either String a

-- | Runtime environment: maps variable names to evaluated values
type Env = M.HashMap Name Value
