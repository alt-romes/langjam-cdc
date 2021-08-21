module Core where

data Decl = Decl {
    dname :: String,
    dcomment :: [Comment],
    dexp :: Expr
                 }
    deriving (Show)


newtype Comment = Comment String
    deriving (Show)


data Expr = Lit Literal
          | Var String
          | Lambda String Expr
          | App Expr Expr

          | Commt String -- Comment value, which can 

data Literal = I Integer | C Char | S String
    deriving (Show)


instance Show Expr where
    show (Lit i) = show i
    show (Var x) = x
    show (Lambda x e) = "(\\" ++ x ++ " -> " ++ show e ++ ")"
    show (App e1 e2) = show e1 ++ " " ++ show e2
    show (Commt s) = s
