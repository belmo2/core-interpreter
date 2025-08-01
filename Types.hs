-- | Here are the types for the Core interpreter.
-- You may add constructors if you wish, but do not modify any that are already here.

module Types where

import qualified Data.HashMap.Lazy as M

-- | Name alias for identifiers
type Name = String

-- | Flag to indicate whether a let is recursive
type IsRec = Bool

-- | Core language expressions
data Expr
  = EVar Name                   -- Variables
  | ENum Int                    -- Numbers
  | EPack Int Int              -- Constructors
  | EAp Expr Expr              -- Applications
  | ELet
      IsRec                    -- is the let recursive?
      [(Name, Expr)]           -- Local Variable definitions
      Expr                     -- Body of the let
  | ECase                      -- Case Expressions
      Expr
      [(Int, [Name], Expr)]    -- Alternatives
  | ELam [Name] Expr           -- Lambda (anonymous functions)
  deriving (Eq, Show)

-- | Supercombinator declarations
type Decl = (Name, [Name], Expr)

-- | A complete Core program
type Core = M.HashMap Name Decl

-- | Runtime values
data Value
  = VInt Int
  | VPack Int [Value]
  | VFun (Value -> Eval Value)

-- | Result of evaluation
type Eval a = Either String a

-- | Runtime environment
type Env = M.HashMap Name Value

-- | Custom show instance to allow printing values
instance Show Value where
  show (VInt i) = show i
  show (VPack tag vals) = "Pack{" ++ show tag ++ ", " ++ show vals ++ "}"
  show (VFun _) = "<function>"
