module Core where

data Decl = Decl { dname :: String, dexp :: Expr }
    deriving (Show)


data Expr = Commt String
          | Presentation [String]

          | Lit Literal
          | Var String
          | Lambda String Expr
          | App Expr Expr

data Literal = I Integer | C Char deriving (Show)


instance Show Expr where
    show (Commt s) = s
    show (Presentation s) = show s

    show (Lit x) = show x
    show (Var x) = x
    show (Lambda x e) = "with " ++ x ++ " do " ++ show e ++ " please"
    show (App x y) = show x ++ " " ++ show y

