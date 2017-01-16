module Translate (
  TranslateError,
  translate
  ) where
import Binding
import Parser
import Primitive
import Control.Applicative
import qualified Data.Map.Strict as Map

type TranslateError = EvalError Evaluator

translate :: NumExpr -> TranslateError

translate (Constant x) = return $ Evaluator $ \_ -> return x

translate (Variable name) =
  return $ Evaluator $ \bindings -> do
    e <- getScalar name bindings
    runEvaluator e bindings

translate (UnaryExpr op expr) = do
  func <- lookupUnary op
  e <- translate expr
  return $ Evaluator $ \bindings -> liftA func (runEvaluator e bindings)

translate (BinaryExpr op expr1 expr2) = do
  func <- lookupBinary op
  e1 <- translate expr1
  e2 <- translate expr2
  return $ Evaluator $ \bindings ->
    liftA2 func (runEvaluator e1 bindings) (runEvaluator e2 bindings)

translate (CondExpr cond ifTrue ifFalse) = do
  t <- translate cond
  e1 <- translate ifTrue
  e2 <- translate ifFalse
  return $ Evaluator $ \bindings -> do
    t' <- runEvaluator t bindings
    if t' /= 0 then runEvaluator e1 bindings
      else runEvaluator e2 bindings

translate (FuncCall func args) = mapM translate args >>= translateFuncCall func


translateFuncCall :: Function -> [Evaluator] -> TranslateError

translateFuncCall (FuncRef name) args =
  case length args of
    1 -> case Prelude.lookup name builtInFuncs of
      Just func -> return $ Evaluator $ \bindings ->
        liftA func (runEvaluator (head args) bindings)
      _ -> translateUserFuncCall name args
    2 -> case Prelude.lookup name builtInFuncs2 of
      Just func -> return $ Evaluator $ \bindings ->
        liftA2 func (runEvaluator (head args) bindings)
                    (runEvaluator (args !! 1) bindings)
      _ -> translateUserFuncCall name args
    _ -> translateUserFuncCall name args

translateFuncCall (Lambda params expr) args = do
  func <- translate expr
  return $ Evaluator $ \bindings -> bindAndEval func params args bindings


translateUserFuncCall :: String -> [Evaluator] -> TranslateError
translateUserFuncCall name args =
  return $ Evaluator $ \bindings -> do
    (func, params) <- getFunction name bindings
    bindAndEval func params args bindings

bindAndEval :: Evaluator -> [String] -> [Evaluator] -> Bindings -> EvalError Double
bindAndEval func params args bindings =
  let b' = foldl (\b (k, v) -> bindScalar k v b) bindings $ zip params args
  in
    runEvaluator func b'
