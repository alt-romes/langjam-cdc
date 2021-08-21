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
    many $ do
        n <- identifier
        reservedOp ":"
        c <- option Nothing $ Just <$> between (reservedOp "##") (reservedOp "##") (many $ satisfy ('#' /=))
        (e,cs) <- option (maybe (error "required at least a comment after a name") Commt c, []) pexp
        reserved "!"
        return $ Decl n (maybe cs ((:cs) . Comment) c) e


pexp, pnotapp, plam, pint, pvar :: Parser (Expr, [Comment])
pexp = do
    (x, cs) <- pnotapp
    (many1 pnotapp >>= \xs -> return (foldl App x (map fst xs), cs ++ concatMap snd xs)) <|> return (x, cs)

-- TODO: [Comment] part which right now is ignored
pnotapp =
    plam <|> pint <|> pvar <|> parens pexp
plam = do
    reservedOp "\\"
    n <- identifier
    reservedOp "->"
    first (Lambda n) <$> pexp
pint = (, []) . Lit . I <$> integer
pvar = (, []) . Var <$> identifier
