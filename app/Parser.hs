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
        reserved "!"
        return $ Decl n (maybe (error "required a comment") Commt c)
    
    do
        reserved "main"
        reservedOp ":"
        (:[]) . Decl "main" . Presentation <$> many identifier
