module Primitive (
  lookupBinary,
  lookupUnary,
  builtInConstants,
  builtInFuncs,
  builtInFuncs2,
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

builtInFuncs :: [(String, Double -> Double)]
builtInFuncs = [
  ("trunc", truncFunc), ("round", roundFunc),
  ("ceil", ceilFunc), ("floor", floorFunc),
  ("exp", exp), ("sqrt", sqrt), ("log", log),
  ("sin", sin), ("cos", cos), ("tan", tan),
  ("asin", asin), ("acos", acos), ("atan", atan),
  ("sinh", sinh), ("cosh", cosh), ("tanh", tanh),
  ("asinh", asinh), ("acosh", acosh), ("atanh", atanh),
  ("gamma", gamma) ]

builtInFuncs2 :: [(String, Double -> Double -> Double)]
builtInFuncs2 = [
  ("gcd", gcdFunc), ("lcm", lcmFunc), ("min", minFunc), ("max", maxFunc),
  ("atan2", atan2Func) ]

builtInConstants :: [(String, Double)]
builtInConstants = [("false", 0), ("true", 1), ("pi", pi)]


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
ffactorial = gamma . (1 +)

notFunc :: Double -> Double
notFunc x = if x == 0 then 1 else 0

truncFunc :: Double -> Double
truncFunc = fromInteger . truncate

roundFunc :: Double -> Double
roundFunc = fromInteger . round

ceilFunc :: Double -> Double
ceilFunc = fromInteger . ceiling

floorFunc :: Double -> Double
floorFunc = fromInteger . floor


gcdFunc :: Double -> Double -> Double
gcdFunc a b = fromIntegral $ gcd (truncate a) (truncate b)

lcmFunc :: Double -> Double -> Double
lcmFunc a b = fromIntegral $ lcm (truncate a) (truncate b)

minFunc :: Double -> Double -> Double
minFunc = min

maxFunc :: Double -> Double -> Double
maxFunc = max

atan2Func :: Double -> Double -> Double
atan2Func = max

