module Main where

import Text.Parsec.String

import Core
import Lexer
import Parser

---- Eval ----
eval :: [Decl] -> Expr -> Expr
eval _ (Commt s) = Commt s
eval ds (Presentation s) = error $ show ds ++ show s
---- ---- ----


main = do
    result <- parseFromFile (contents pmanydecl) "test.txt"
    case result of
      Left err    -> print err
      Right decls ->
        case lookup "main" $ map (\d -> (dname d, dexp d)) decls of
          Just e -> print $ eval decls e
          Nothing -> print "No main function defined"
