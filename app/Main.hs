module Main where

import Text.Parsec.String

import Core
import Lexer
import Parser

---- Eval ----
eval :: [Decl] -> Expr -> Expr
eval _ (Lit i) = Lit i
eval decls (Var x) = case lookup x $ map (\d -> (dname d, dexp d)) decls of
                   Just d -> eval decls d
                   Nothing -> Var x
eval _ (Lambda x e) = Lambda x e
eval ds (App e1 e2) = do
    let Lambda n e1' = eval ds e1
    case eval ds e1 of
      Lambda n e1' ->
        let arg = eval ds e2 in
        eval ds $ substitute n arg e1'
      Commt s ->
          let Commt s' = eval ds e2 in
          Commt (s ++ "\n---\n" ++ s')

eval _ (Commt s) = Commt s


substitute :: String -> Expr -> Expr -> Expr
substitute n arg (Var x) = if n == x then arg else Var x
substitute n arg (Lambda x e) = Lambda x $ substitute n arg e -- this is fundamentally wrong :)
substitute n arg (App e1 e2) = App (substitute n arg e1) (substitute n arg e2)
substitute n arg x = x
---- ---- ----


main = do
    result <- parseFromFile (contents pmanydecl) "test.txt"
    case result of
      Left err    -> print err
      Right decls ->
        case lookup "main" $ map (\d -> (dname d, dexp d)) decls of
          Just e -> print $ eval decls e
          Nothing -> print "No main function defined"
