module Core where

data Decl = Decl { dname :: String, dexp :: Expr }
    deriving (Show)


data Expr = Commt String
          | Presentation [String]
                


instance Show Expr where
    show (Commt s) = s
