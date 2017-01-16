module NumEval.Syntax (
  NumExpr(..),
  UnaryOp(..),
  BinaryOp(..),
  Function(..)
  ) where

data NumExpr = Constant Double
             | Variable String
             | UnaryExpr UnaryOp NumExpr
             | BinaryExpr BinaryOp NumExpr NumExpr
             | CondExpr NumExpr NumExpr NumExpr
             | FuncCall Function [NumExpr]
             deriving (Show)

data UnaryOp = Factorial
             | Negate
             | Not
             deriving (Eq, Show)

data BinaryOp = Add | Subtract | Multiply | Divide | Modulo | Pow
              | And | Or
              | RelLT | RelLE | RelEQ | RelGE | RelGT | RelNE
              deriving (Eq, Show) 

data Function = FuncRef String
              | Lambda [String] NumExpr
              deriving (Show)
