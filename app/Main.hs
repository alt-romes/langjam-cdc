import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Monad
import Data.List

data Decl = Decl {
    dname :: String,
    dcomment :: Comment,
    dexp :: Expr
                 }
    deriving (Show)

newtype Comment = Comment String
    deriving (Show)

data Expr = Const Integer
          | Var String
          | Lambda String Expr
          | App Expr Expr

eval :: [Decl] -> Expr -> Expr
eval _ (Const i) = Const i
eval decls (Var x) = case lookup x $ map (\d -> (dname d, dexp d)) decls of
                   Just d -> eval decls d
                   Nothing -> Var x
eval _ (Lambda x e) = Lambda x e
eval ds (App e1 e2) = do
    let Lambda n e1' = eval ds e1
    let arg = eval ds e2
    eval ds $ substitute n arg e1'

substitute :: String -> Expr -> Expr -> Expr
substitute n arg (Var x) = if n == x then arg else Var x
substitute n arg (Lambda x e) = Lambda x $ substitute n arg e
substitute n arg (App e1 e2) = App (substitute n arg e1) (substitute n arg e2)
substitute n arg x = x

instance Show Expr where
    show (Const i) = show i
    show (Var x) = x
    show (Lambda x e) = "(\\" ++ x ++ " -> " ++ show e ++ ")"
    show (App e1 e2) = show e1 ++ " " ++ show e2



---- Parser ----
pmanydecl :: Parser [Decl] 
pmanydecl = many $ do
    c <- option "" $ do
        reservedOp "{-" 
        c <- concat <$> many identifier
        reservedOp "-}"
        return c
    n <- identifier
    reservedOp ":"
    e <- pexp
    reserved "lol"
    return $ Decl n (Comment c) e

pexp, pnotapp, plam, pint, pvar :: Parser Expr
pexp = pnotapp >>= \x ->
    (many1 pnotapp >>= \xs -> return (foldl App x xs)) <|> return x
pnotapp = plam <|> pint <|> pvar <|> parens pexp
plam = do
    reservedOp "\\"
    n <- identifier
    reservedOp "->"
    Lambda n <$> pexp
pint = Const <$> integer
pvar = Var <$> identifier
---- ----- ----


---- Lexer ----
lexer = P.makeTokenParser $ emptyDef {
    P.reservedNames = ["lol"],
    P.reservedOpNames = ["\\", "->", "{-", "-}", ":"] 
                                     }
parens = P.parens lexer
integer = P.integer lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
---- ----- ----

main = do
    result <- parseFromFile pmanydecl "show.txt"
    case result of
      Left err    -> print err
      Right decls ->
        case lookup "main" $ map (\d -> (dname d, dexp d)) decls of
          Just e -> print $ eval decls e
          Nothing -> print "No main function defined"
