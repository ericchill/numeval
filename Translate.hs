module Translate (
  TranslateError,
  translate
  ) where
import Parser
import Primitive
import VarStore
import Control.Applicative

type TranslateError a = EvalError (Evaluator a)

translate :: (VarStore a) => NumExpr -> TranslateError a

translate (Constant x) = return $ Evaluator $ \_ _ -> return x

translate (Variable name) =
  return $ Evaluator $ \vars _ -> do
  case getVar name vars of
    Just e -> (runEvaluator e) vars []
    Nothing -> throwError $ name ++ " is undefined."

translate (UnaryExpr op e) = do
  func <- lookupUnary op
  e' <- translate e
  return $ Evaluator $ \vars args -> liftA func ((runEvaluator e') vars args)

translate (BinaryExpr op e1 e2) = do
  func <- lookupBinary op
  e1' <- translate e1
  e2' <- translate e2
  return $ Evaluator $ \vars args ->
    liftA2 func ((runEvaluator e1') vars args) ((runEvaluator e2') vars args)

translate (CondExpr test e1 e2) = do
  test' <- translate test
  e1' <- translate e1
  e2' <- translate e2
  return $ Evaluator $ \vars args -> do
    t1 <- (runEvaluator test') vars args
    if t1 /= 0 then (runEvaluator e1') vars args
      else (runEvaluator e2') vars args

translate (FuncCall func args) = mapM translate args >>= translateFunc func


translateFunc :: (VarStore a) => Function -> [Evaluator a] -> TranslateError a

translateFunc (FuncRef name) args =
  return $ Evaluator $ \vars _ -> do
    case getVar name vars of
      Just func -> do
        --args' <- evalArgs args vars
        (runEvaluator func) vars args
      Nothing -> throwError $ "Function " ++ name ++ " undefined."

translateFunc (Lambda params expr) args = do
  expr' <- translate expr
  return $ Evaluator $ \vars _ -> do
    let vars' = foldl (\vs (k, v) -> setVar k v vs) vars $ zip params args in
      (runEvaluator expr') vars' []


evalArgs :: (VarStore a) => [Evaluator a] -> a -> EvalError [Double]
evalArgs args vars = mapM (\arg -> (runEvaluator arg) vars []) args

constEvaluator :: (VarStore a) => Double -> Evaluator a
constEvaluator x = Evaluator $ \_ _ -> return x
