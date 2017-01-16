module NumEval.Binding (
  EvalError,
  Evaluator(..),
  Bindings(..),
  defaultBindings,
  bindScalar,
  getScalar,
  getFunction,
  module Control.Monad.Except
  ) where
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as Map

type EvalError = Either String

newtype Evaluator = Evaluator {
  runEvaluator :: Bindings -> EvalError Double
  }

type BoundValue = Either Evaluator (Evaluator, [String])

type Bindings = Map.Map String BoundValue

defaultBindings :: Bindings
defaultBindings =
  foldl (\b (k, v) -> Map.insert k (Left $ Evaluator $ \_ -> return v) b)
  Map.empty
  [("false", 0), ("true", 1), ("pi", pi)]

bindScalar :: String -> Evaluator -> Bindings -> Bindings
bindScalar name eval = Map.insert name (Left eval)

getScalar :: String -> Bindings -> EvalError Evaluator
getScalar name bindings = do
  bound <- getBinding name bindings
  case bound of
    Left eval -> return eval
    _ -> throwError $ show name ++ " is not bound to a scalar."
    
getFunction :: String -> Bindings -> EvalError (Evaluator, [String])
getFunction name bindings = do
  bound <- getBinding name bindings
  case bound of
    Right bound -> return bound
    _ -> throwError $ show name ++ " is not bound to a function."

getBinding :: String -> Bindings -> EvalError BoundValue
getBinding name bindings =
  case Map.lookup name bindings of
    Just x  -> return x
    Nothing -> throwError $ show name ++ " is not bound."
