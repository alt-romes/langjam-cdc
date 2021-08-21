module Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer = P.makeTokenParser $ emptyDef {
    P.reservedNames = [],
    P.reservedOpNames = ["\\", "->", ":", "!", "#", "##"] 
                                     }
parens = P.parens lexer
integer = P.integer lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

contents p = do
  P.whiteSpace lexer
  r <- p
  eof
  return r
