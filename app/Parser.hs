{-# LANGUAGE TupleSections #-}
module Parser where

import Text.Parsec
import Control.Monad
import Data.Bifunctor
import Data.List

import Core
import Lexer

type Parser = Parsec String ()

pmanydecl :: Parser [Decl] 
pmanydecl = do
    -- top level declarations
    cs <- many $ do
        reservedOp "#"
        n <- identifier
        reservedOp "#"
        -- c <- option Nothing $ Just <$> between (reservedOp "#") (reservedOp "##") (many $ satisfy ('#' /=))
        c <- many $ satisfy ('#' /=)
        reservedOp "##"
        return $ Decl n (Commt c)
    ls <- many $ do
        n <- identifier
        reservedOp ":"
        e <- pexp
        reservedOp "!"
        return $ Decl n e
    do
        reserved "main"
        reservedOp ":"
        (((cs++ls)++) . (:[])) . Decl "main" . Presentation <$> many identifier

pexp :: Parser Expr
pexp = do
     x <- pnotapp
     (do 
         xs <- many1 pnotapp
         return (foldl App x xs)) <|> return x

pnotapp, plam, pint, pvar :: Parser Expr
pnotapp = plam <|> pint <|> pchar <|> pvar <|> parens pexp

plam = do
     reserved "with"
     n <- identifier
     reserved "do"
     e <- pexp
     reserved "please"
     return $ Lambda n e
pint = Lit . I <$> integer
pvar = Var <$> identifier
pchar = do
    char '\''
    c <- letter
    char '\''
    return $ Lit $ C c
