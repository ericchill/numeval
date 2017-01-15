module Primitive (
  lookupBinary,
  lookupUnary,
  unaryFuncs,
  ffactorial
  ) where
import Syntax
import Control.Monad.Except
import Control.Monad.Identity
import Math.Gamma

type EvalError = Either String

lookupBinary :: BinaryOp -> EvalError (Double -> Double -> Double)
lookupBinary op =
  case lookup op binaryFuncs of
    Just func -> return func
    Nothing   -> throwError $ "No function for operator " ++ show op

binaryFuncs :: [(BinaryOp, Double -> Double -> Double)]
binaryFuncs = [
  (Add, (+)),      (Subtract, (-)), (Multiply, (*)), (Divide, (/)),
  (Modulo, fmod),  (Pow, (**)),
  (RelLT, relFunc (<)),  (RelLE, relFunc (<=)), (RelEQ, relFunc (==)),
  (RelGE, relFunc (>=)), (RelGT, relFunc (>)),  (RelNE, relFunc (/=)),
  (And, boolFunc (&&)),  (Or, boolFunc (||))
  ]

lookupUnary :: UnaryOp -> EvalError (Double -> Double)
lookupUnary op =
  case lookup op unaryFuncs of
    Just func -> return func
    Nothing   -> throwError $ "No function for operator " ++ show op


unaryFuncs :: [(UnaryOp, Double -> Double)]
unaryFuncs = [ (Negate, negate), (Factorial, ffactorial), (Not, notFunc) ]

relFunc :: (Double -> Double -> Bool) -> Double -> Double -> Double
relFunc func a b =
  if a `func` b then 1 else 0

boolFunc :: (Bool -> Bool -> Bool) -> Double -> Double -> Double
boolFunc func a b =
  let a' = a /= 0
      b' = b /= 0
  in
    if a' && b' then 1.0 else 0.0


fmod :: Double -> Double -> Double
fmod n d = n - d * fromIntegral (round $ n / d)

ffactorial :: Double -> Double
ffactorial x = gamma $ x + 1

notFunc :: Double -> Double
notFunc x = if x == 0 then 1 else 0
