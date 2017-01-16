module NumEval (
  eval,
  runable
  ) where
import Parser
import Translate
import VarStore
import qualified Data.Map.Strict as Map

eval :: String -> String -> EvalError Double
eval ctx s = do
  e <- runable ctx s
  runEvaluator e Map.empty []

runable :: String -> String -> EvalError Evaluator
runable ctx s =
  case parseExpr ctx s of
    Left err   -> throwError $ show err
    Right expr -> translate expr

