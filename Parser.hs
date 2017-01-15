module Parser (
  numExpr,
  module Syntax
  ) where
import Lexer
import Syntax
import System.IO
import Control.Monad
import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

numExpr = do
  e <- numExpr'
  e' <- optionMaybe ternary
  case e' of
    Nothing -> return e
    Just (ifTrue, ifFalse) -> return $ CondExpr e ifTrue ifFalse
  
ternary = do
  reservedOp "?"
  ifTrue <- numExpr
  colon
  ifFalse <- numExpr
  return (ifTrue, ifFalse)

numExpr' = buildExpressionParser numTable numTerm <?> "numexpr"

numTable = [ [Prefix  (reservedOp "-"  >> return (UnaryExpr  Negate    ))            ]
           , [Postfix (reservedOp "!"  >> return (UnaryExpr  Factorial ))            ]
           , [Infix   (reservedOp "^"  >> return (BinaryExpr Pow       )) AssocRight ]
           , [Infix   (reservedOp "*"  >> return (BinaryExpr Multiply  )) AssocLeft  ]
           , [Infix   (reservedOp "/"  >> return (BinaryExpr Divide    )) AssocLeft  ]
           , [Infix   (reservedOp "%"  >> return (BinaryExpr Modulo    )) AssocLeft  ]
           , [Infix   (reservedOp "+"  >> return (BinaryExpr Add       )) AssocLeft  ]
           , [Infix   (reservedOp "-"  >> return (BinaryExpr Subtract  )) AssocLeft  ]
           , [Infix   (reservedOp "<"  >> return (BinaryExpr RelLT     )) AssocNone  ]
           , [Infix   (reservedOp "<=" >> return (BinaryExpr RelLE     )) AssocNone  ]
           , [Infix   (reservedOp "==" >> return (BinaryExpr RelEQ     )) AssocNone  ]
           , [Infix   (reservedOp ">=" >> return (BinaryExpr RelGE     )) AssocNone  ]
           , [Infix   (reservedOp ">"  >> return (BinaryExpr RelGT     )) AssocNone  ]
           , [Infix   (reservedOp "!=" >> return (BinaryExpr RelNE     )) AssocNone  ]
           , [Prefix  (reservedOp "~"  >> return (UnaryExpr  Not       ))            ]
           , [Infix   (reservedOp "&&" >> return (BinaryExpr And       )) AssocLeft  ]
           , [Infix   (reservedOp "||" >> return (BinaryExpr Or        )) AssocLeft  ] ]

numTerm =  parens numTerm2
       <|> liftM Variable identifier
       <|> liftM Constant float

numTerm2 = funcCall <|> lambdaCall <|> numExpr

funcCall = do
  name <- identifier
  args <- parens $ commaSep1 numExpr
  return $ FuncCall (FuncRef name) args

lambdaCall = do
  def <- braces lambdaDef
  args <- parens $ commaSep1 numExpr
  return $ FuncCall def args

lambdaDef = do
  params <- parens $ commaSep1 identifier
  reservedOp ":"
  def <- numExpr
  return $ Lambda params def

