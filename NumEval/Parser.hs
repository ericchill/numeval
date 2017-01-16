module NumEval.Parser (
  parseExpr,
  grammar,
  expr
  ) where
import NumEval.Lexer
import NumEval.Syntax
import System.IO
import Control.Monad
import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Debug.Trace

parseExpr :: String -> String -> Either ParseError NumExpr
parseExpr = parse grammar

grammar = do
  whitespace
  expr <- expr
  eof
  return expr

expr = do
  e <- expr'
  e' <- optionMaybe ternary
  case e' of
    Nothing -> return e
    Just (ifTrue, ifFalse) -> return $ CondExpr e ifTrue ifFalse
  
ternary = do
  reservedOp "?"
  ifTrue <- expr
  colon
  ifFalse <- expr
  return (ifTrue, ifFalse)

expr' = buildExpressionParser opTable term <?> "numexpr"

opTable =
  [ [Prefix  (reservedOp "-"  >> return (UnaryExpr  Negate    ))            ]
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

term =  parens expr
       <|> lambdaCall
       <|> funcCall
       <|> liftM Variable identifier
       <|> liftM Constant (try float)
       <|> liftM (Constant . fromInteger) integer

funcCall = try $ do
  name <- identifier
  args <- parens $ commaSep1 expr
  return $ FuncCall (FuncRef name) args

lambdaCall = try $ do
  def <- braces lambdaDef
  args <- parens $ commaSep1 expr
  return $ FuncCall def args

lambdaDef = do
  params <- parens $ commaSep1 identifier
  colon
  def <- expr
  return $ Lambda params def

