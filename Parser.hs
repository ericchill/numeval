module Parser where
import System.IO
import Control.Monad
import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data NumExpr = NumConst Double
             | NumVar String
             | NumUnOp NUnOp NumExpr
             | NumBinOp NBinOp NumExpr NumExpr
             | NumTest BoolExpr NumExpr NumExpr
             | NumFunc Function [NumExpr]

data BoolExpr = BoolConst Bool
              | BoolNot BoolExpr
              | BoolBinOp BBinOp BoolExpr BoolExpr
              | BoolRelExpr RelExpr

data RelExpr = RelTerm NumExpr | RelBinOp RelOp RelExpr RelExpr

data Function = NamedFunc String
              | NamelessFunc [String] NumExpr

data NUnOp = Negate | Factorial

data NBinOp = Add | Subtract | Multiply | Divide | Modulo | Pow

data BBinOp = And | Or

data RelOp = RelLT | RelLE | RelEQ | RelGE | RelGT | RelNE

languageDef :: LanguageDef st
languageDef =
  let java = javaStyle in
    java { Token.opStart = opStart emptyDef,
           Token.opLetter = opLetter emptyDef,
           Token.reservedNames = ["gcd", "lcm", "pi", "exp", "sqrt", "log",
                                  "sin", "tan", "cos", "asin", "atan", "acos",
                                  "sinh", "tanh", "cosh", "asinh", "atanh", "acosh",
                                  "trunc", "round", "ceil", "floor",
                                  "true", "false"],
           Token.reservedOpNames = ["+", "-", "*", "/", "%", "^", "!",
                                    "<", "<=", "==", ">=", ">", "!=",
                                     "?", ":",
                                     "&&", "||", "~"]
         }

type TokenParser st = Token.GenTokenParser String st Identity

--lexer :: TokenParser st
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
braces     = Token.braces     lexer
colon      = Token.colon      lexer
comma      = Token.comma      lexer
commaSep1  = Token.commaSep1  lexer
parens     = Token.parens     lexer
float      = Token.float      lexer
whitespace = Token.whiteSpace lexer


numExpr :: Parser NumExpr
numExpr = buildExpressionParser numTable numTerm <?> "numexpr"

numTable = [ [Prefix  (reservedOp "-" >> return (NumUnOp Negate     ))            ]
           , [Postfix (reservedOp "!" >> return (NumUnOp Factorial  ))            ]
           , [Infix   (reservedOp "^" >> return (NumBinOp Pow       )) AssocRight ]
           , [Infix   (reservedOp "*" >> return (NumBinOp Multiply  )) AssocLeft  ]
           , [Infix   (reservedOp "/" >> return (NumBinOp Divide    )) AssocLeft  ]
           , [Infix   (reservedOp "%" >> return (NumBinOp Modulo    )) AssocLeft  ]
           , [Infix   (reservedOp "+" >> return (NumBinOp Add       )) AssocLeft  ]
           , [Infix   (reservedOp "-" >> return (NumBinOp Subtract  )) AssocLeft  ] ]

numTerm =  parens numTerm2
       <|> liftM NumVar identifier
       <|> liftM NumConst float

numTerm2 = funcCall <|> namelessFuncCall <|> numConditional <|> numExpr

funcCall = do
  name <- identifier
  args <- parens $ commaSep1 numExpr
  return $ NumFunc (NamedFunc name) args

namelessFuncCall = do
  def <- braces namelessDef
  args <- parens $ commaSep1 numExpr
  return $ NumFunc def args

namelessDef = do
  params <- parens $ commaSep1 identifier
  reservedOp ":"
  def <- numExpr
  return $ NamelessFunc params def

numConditional = do
  test <- boolExpr
  reservedOp "?"
  ifTrue <- numExpr
  reservedOp ":"
  ifFalse <- numExpr
  return $ NumTest test ifTrue ifFalse

boolExpr :: Parser BoolExpr
boolExpr = buildExpressionParser boolTable boolTerm <?> "boolexpr"

boolTable = [ [Prefix (reservedOp "~"  >> return (BoolNot            ))           ]
            , [Infix  (reservedOp "&&" >> return (BoolBinOp And      )) AssocLeft ]
            , [Infix  (reservedOp "||" >> return (BoolBinOp Or       )) AssocLeft ] ]

boolTerm = relation <|> parens boolExpr

relation = do
  expr <- relExpr
  return $ BoolRelExpr expr

relExpr :: Parser RelExpr
relExpr = buildExpressionParser relTable relTerm <?> "relexpr"

relTable = [ [Infix  (reservedOp "<"  >> return (RelBinOp RelLT )) AssocNone ]
           , [Infix  (reservedOp "<=" >> return (RelBinOp RelLE )) AssocNone ]
           , [Infix  (reservedOp "==" >> return (RelBinOp RelEQ )) AssocNone ]
           , [Infix  (reservedOp ">=" >> return (RelBinOp RelGE )) AssocNone ]
           , [Infix  (reservedOp ">"  >> return (RelBinOp RelGT )) AssocNone ]
           , [Infix  (reservedOp "!=" >> return (RelBinOp RelNE )) AssocNone ] ]

relTerm =  parens relExpr
       <|> liftM RelTerm numExpr
