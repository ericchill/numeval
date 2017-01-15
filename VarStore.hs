module VarStore (
  EvalError,
  Evaluator(..),
  VarStore(..),
  module Control.Monad.Except
  ) where
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as Map

type EvalError = Either String

type VarStore = Map.Map String Evaluator

newtype Evaluator = Evaluator {
  runEvaluator :: VarStore ->       -- Named variables
                  [Evaluator] ->    -- Positional arguments
                  EvalError Double
  }

