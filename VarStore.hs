module VarStore (
  EvalError,
  Evaluator(..),
  VarStore(..),
  module Control.Monad.Except
  ) where
import Control.Monad.Except
import Control.Monad.Identity

type EvalError = ExceptT String Identity

newtype Evaluator a = Evaluator {
  runEvaluator :: a ->                -- Named variables
                  [Evaluator a] ->    -- Positional arguments
                  EvalError Double
  }

class VarStore a where
  getVar :: String -> a -> Maybe (Evaluator a)
  setVar :: String -> Evaluator a -> a -> a

