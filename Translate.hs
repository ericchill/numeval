module Translate (
  TranslateError,
  translate
  ) where
import Parser
import Primitive
import VarStore
import Control.Applicative
import qualified Data.Map.Strict as Map

type TranslateError = EvalError Evaluator

translate :: NumExpr -> TranslateError

translate (Constant x) = return $ Evaluator $ \_ _ -> return x

translate (Variable name) =
  case Prelude.lookup name builtInConstants of
    Just x -> return $ Evaluator $ \_ _ -> return x
    _ -> return $ Evaluator $ \vars _ ->
      case Map.lookup name vars of
        Just e -> runEvaluator e vars []
        Nothing -> throwError $ "Variable " ++ show name ++ " is undefined."

translate (UnaryExpr op e) = do
  func <- lookupUnary op
  e' <- translate e
  return $ Evaluator $ \vars args -> liftA func (runEvaluator e' vars args)

translate (BinaryExpr op e1 e2) = do
  func <- lookupBinary op
  e1' <- translate e1
  e2' <- translate e2
  return $ Evaluator $ \vars args ->
    liftA2 func (runEvaluator e1' vars args) (runEvaluator e2' vars args)

translate (CondExpr test e1 e2) = do
  test' <- translate test
  e1' <- translate e1
  e2' <- translate e2
  return $ Evaluator $ \vars args -> do
    t1 <- runEvaluator test' vars args
    if t1 /= 0 then runEvaluator e1' vars args
      else runEvaluator e2' vars args

translate (FuncCall func args) = mapM translate args >>= translateFunc func


translateFunc :: Function -> [Evaluator] -> TranslateError

translateFunc (FuncRef name) args =
  case length args of
    1 -> case Prelude.lookup name builtInFuncs of
      Just func -> return $ Evaluator $ \vars fooArgs ->
        liftA func (runEvaluator (head args) vars fooArgs)
      _ -> translateUserFuncCall name args
    2 -> case Prelude.lookup name builtInFuncs2 of
      Just func -> return $ Evaluator $ \vars fooArgs ->
        liftA2 func (runEvaluator (head args) vars fooArgs)
                    (runEvaluator (args !! 1) vars fooArgs)
      _ -> translateUserFuncCall name args
    _ -> translateUserFuncCall name args

translateFunc (Lambda params expr) args = do
  expr' <- translate expr
  return $ Evaluator $ \vars _ ->
    let vars'
          = foldl (\vs (k, v) -> Map.insert k v vs) vars $ zip params args
      in runEvaluator expr' vars' []


translateUserFuncCall name args =
  return $ Evaluator $ \vars fooArgs ->
  case Map.lookup name vars of
    Just func -> runEvaluator func vars args
    Nothing   -> throwError $ "Function " ++ show name ++ " is undefined " ++
      "(at least for " ++ show (length args) ++ " arguments)."
