-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M
import Types

--- ### Evaluation Monad and Environment

type Eval a = Either String a

--- ### Value type and Environment

type Env = M.HashMap Name Value

--- ### Evaluator

eval :: Expr -> Env -> Core -> Eval Value

eval (ENum i) _ _ = return $ VInt i

eval (EVar x) env core =
  case M.lookup x env of
    Just v  -> return v
    Nothing ->
      case M.lookup x core of
        Just (_, params, body) ->
          -- Treat supercombinators as global multi-arg closures!
          return $ makeClosure params body env core
        Nothing -> Left $ "Unbound variable: " ++ x

eval (ELam params body) env core =
  case params of
    [] -> Left "Lambda must have at least one parameter"
    _  -> return $ makeClosure params body env core

eval (EAp f x) env core = do
  vf <- eval f env core
  vx <- eval x env core
  case vf of
    VFun fun -> fun vx
    _ -> Left "Trying to apply a non-function, wrong"

-- Placeholders for remaining forms:
eval (ELet isRec defs body) env core =
  if not isRec
    then do
      env' <- foldM (\e (n, ex) -> eval ex env core >>= \v -> return $ M.insert n v e) env defs
      eval body env' core
    else
      -- letrec: mutually recursive, using Haskell laziness
      let env' = M.union (M.fromList [(n, unsafeEval ex env' core) | (n, ex) <- defs]) env
      in eval body env' core
  where
    unsafeEval ex e c = case eval ex e c of
                          Right v -> v
                          Left err -> error err

eval (EPack tag arity) env core =
  -- This is a stub: in real Core, arguments will be passed via EAp (see ECase)
  return $ VPack tag []

eval (ECase expr alts) env core = do
  v <- eval expr env core
  case v of
    VPack tag vs -> matchAlt tag vs alts
    _ -> Left "Case on non-constructor"
  where
    matchAlt _ _ [] = Left "No matching case alternative, no soup for you."
    matchAlt tag vs ((tag', params, rhs):rest)
      | tag == tag' && length params == length vs =
          let env' = M.union (M.fromList (zip params vs)) env
          in eval rhs env' core
      | otherwise = matchAlt tag vs rest

--- ### Lambda/Closure Helper

makeClosure :: [Name] -> Expr -> Env -> Core -> Value
makeClosure [] body env _ = error "This Mission was Impossible: empty lambda"
makeClosure (p:ps) body env core = VFun $ \arg ->
  let env' = M.insert p arg env
  in if null ps
     then eval body env' core
     else return $ makeClosure ps body env' core

--- ### Top-Level Entry Point

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_, [], mainBody) ->
      case eval mainBody M.empty prog of
        Right (VInt n) -> show n
        Right _ -> error "Main did not return an integer."
        Left err -> error $ "Runtime error: " ++ err
    Just (_, args, _) ->
      error $ "main should not have parameters, but got: " ++ show args
