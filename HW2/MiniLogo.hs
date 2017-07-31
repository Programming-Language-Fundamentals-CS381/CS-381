module MiniLogo where

--Definitions
data Numb = Int

data Var = String

data Macro = String

data Prog = Null | Cmd; Prog --NEED a construct

data Mode = Down | Up
            deriving Show

data Expr = Var
            | Numb
            | Expr + Expr
            derving Show

data Cmd = Pen Mode
           | Move ( Expr , Expr )
           | define macro ( [Var] ) { Prog }
           | call macro ( [Expr] )
           deriving Show

-- Line :: 
-- Something here as an expression for line
-- nix = 


-- Will do a stair like drawing of the line
steps :: Int -> Prog



pretty :: Prog -> String


-- Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions 
-- by replacing any additions of literals with the result.
-- For example, given the expression (2+3)+x, optE should return the expression 5+x.
optE :: Expr -> Expr


-- Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions 
-- contained in a given program using optE.
optP :: Prog -> Prog




