module Lexer (
  identifier,
  reserved,
  reservedOp,
  braces,
  colon,
  comma,
  commaSep1,
  parens,
  float,
  whitespace
  ) where
import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: LanguageDef st
languageDef =
  emptyDef { Token.reservedNames = ["gcd", "lcm", "pi", "exp", "sqrt", "log",
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

lexer :: TokenParser st
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

