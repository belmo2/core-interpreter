module Interp where

import qualified Data.HashMap.Lazy as M
import Types

import Control.Monad (foldM)  -- needed for the non-recursive let

--- ### Evaluator

eval :: Expr -> Env -> Core -> Eval Value

-- Special lazy if: if cond thenExpr elseExpr to avoid the StackOverflow error
eval (EAp (EAp (EAp (EVar "if") cond) thenE) elseE) env core = do
  vc <- eval cond env core
  case vc of
    VInt c ->
      if c /= 0
        then eval thenE env core
        else eval elseE env core
    _ -> Left "Type error in if condition."

eval (ENum i) _ _ = return $ VInt i

eval (EVar x) env core =
  case M.lookup x env of
    Just v  -> return v
    Nothing ->
      case M.lookup x core of
        Just (_, [], body) -> eval body env core
          -- Treat supercombinators (Not the Earth Supercomputer) as global multi-arg closures!
        Just (_, params, body) -> return $ makeClosure params body env core
        Nothing -> Left $ "Unbound variable: " ++ x

eval (ELam params body) env core =
  case params of
    [] -> Left "Lambda must have at least one parameter."
    _  -> return $ makeClosure params body env core

eval (EAp f x) env core = do
  vf <- eval f env core
  vx <- eval x env core
  case vf of
    VFun fun -> fun vx
    _ -> Left "Trying to apply a non-function."

eval (ELet isRec defs body) env core
  | not isRec = do
      env' <- foldM
        (\e (n, ex) -> do
            v <- eval ex e core
            return $ M.insert n v e)
        env
        defs
      eval body env' core
  | otherwise =
      let env' = M.union recDefs env
          recDefs = M.fromList
            [ (n, val)
            | (n, ex) <- defs
            , let val = case eval ex env' core of
                          Right v  -> v
                          Left err -> error $ "letrec binding error for " ++ n ++ ": " ++ err
            ]
      in eval body env' core

eval (EPack tag arity) _ _ =
  -- This is a stub: in real Core, arguments are added via EAp chains
  return $ VPack tag []

eval (ECase expr alts) env core = do
  v <- eval expr env core
  case v of
    VPack tag vs -> matchAlt tag vs alts
    _ -> Left "Case on non-constructor."
  where
    matchAlt _ _ [] = Left "No matching case alternative.  No soup for you."
    matchAlt tagVal vs ((tag', params, rhs):rest)
      | tagVal == tag' && length params == length vs =
          let pairs =
                [ (p, val)
                | (p, val) <- zip params vs
                , p /= "_"  -- drop wildcards
                ]
              env' = M.union (M.fromList pairs) env
          in eval rhs env' core
      | otherwise = matchAlt tagVal vs rest

--- ### Lambda/Closure Helper

makeClosure :: [Name] -> Expr -> Env -> Core -> Value
makeClosure [] _ _ _ = error "Empty lambda: no parameters given."
makeClosure (p:ps) body env core = VFun $ \arg ->
  let env' = M.insert p arg env
  in if null ps
     then eval body env' core
     else return $ makeClosure ps body env' core

--- ### Primitive Operators

-- | if cond thenVal elseVal  -- cond: integer (0=false, nonzero=true)
ifPrim :: Value
ifPrim = VFun $ \cond -> return $ VFun $ \thenV -> return $ VFun $ \elseV ->
  case cond of
    VInt c -> if c /= 0 then return thenV else return elseV
    _ -> Left "Type error in if condition."

primEnv :: Env
primEnv = M.fromList
  [ ("+", binIntOp (+))
  , ("-", binIntOp (-))
  , ("*", binIntOp (*))
  , ("/", binIntOp div)
  , ("==", binIntPred (==))
  , ("~=", binIntPred (/=))
  , (">", binIntPred (>))
  , (">=", binIntPred (>=))
  , ("<", binIntPred (<))
  , ("<=", binIntPred (<=))
  , ("&", binBoolOp (&&))
  , ("|", binBoolOp (||))
  , ("if", ifPrim)
  ]
-- | Helper: curried binary int -> int
binIntOp :: (Int -> Int -> Int) -> Value
binIntOp op = VFun $ \v1 -> return $ VFun $ \v2 ->
  case (v1, v2) of
    (VInt n1, VInt n2) -> return $ VInt (op n1 n2)
    _ -> Left "Type error in integer operator."

-- | Helper: curried binary int -> bool (as VInt 1 / 0)
binIntPred :: (Int -> Int -> Bool) -> Value
binIntPred op = VFun $ \v1 -> return $ VFun $ \v2 ->
  case (v1, v2) of
    (VInt n1, VInt n2) -> return $ VInt (if op n1 n2 then 1 else 0)
    _ -> Left "Type error in comparison."

-- | Helper: curried binary bool -> bool
binBoolOp :: (Bool -> Bool -> Bool) -> Value
binBoolOp op = VFun $ \v1 -> return $ VFun $ \v2 ->
  case (v1, v2) of
    (VInt b1, VInt b2) ->
      return $ VInt (if op (b1 /= 0) (b2 /= 0) then 1 else 0)
    _ -> Left "Type error in boolean operator."

--- ### Top-Level Entry Point

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator 'main' not defined. (Did you forget to build The Earth?)"
    Just (_, [], mainBody) ->
      case eval mainBody primEnv prog of
        Right (VInt n) -> show n
        Right _ -> error "Main did not return an integer."
        Left err -> error $ "Runtime error: " ++ err
    Just (_, args, _) ->
      error $ "main should not have parameters, but got: " ++ show args
