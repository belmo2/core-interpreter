-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M
import Types

--- ### Evaluation Monad and Environment

-- | The evaluation monad to report runtime errors
type Eval a = Either String a

--- ### Evaluator

-- | The evaluator function
-- Takes an expression and an environment, and evaluates it to a Value
eval :: Expr -> Env -> Eval Value
eval (ENum i) _ = return $ VInt i

eval (EVar x) env =
  case M.lookup x env of
    Just v  -> return v
    Nothing -> Left $ "Unbound variable: " ++ x

-- TODO:
-- - Implement EAp (function application)
-- - Implement ELet and ELet True (let and letrec)
-- - Implement ELam (lambdas)
-- - Implement EPack (constructors)
-- - Implement ECase (pattern matching)

--- ### Top-Level Entry Point

-- | Use this function as your top-level entry point so you don't break `app/Main.hs`
run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_, [], mainBody) ->
      case eval mainBody M.empty of
        Right (VInt n) -> show n
        Right _ -> error "Main did not return an integer."
        Left err -> error $ "Runtime error: " ++ err
    Just (_, args, _) ->
      error $ "main should not have parameters, but got: " ++ show args
