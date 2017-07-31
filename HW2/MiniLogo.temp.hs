module MiniLogo where

import Data.List
import Prelude hiding (Enum (..), Num, Var)

--
-- * MiniLogo
--
-- | The grammar:
--      num ::= (any natural number)
--      var ::= (any variable name)
--    macro ::= (any macro name)
--
--     prog ::= ε | cmd; prog                 sequence of commands
--
--     mode ::= up | down                     pen status
--
--     expr ::= var                           variable reference
--           |  num                           literal number
--           |  expr + expr                   addition expression
--
--      cmd ::= pen mode                      change pen status
--           |  move (expr, expr)             move pen to a new position
--           |  define macro (var*) {prog}    define a macro
--           |  call macro (expr*)            invoke a macro

-- | 1. Define the abstract syntax as a set of Haskell data types.
--

-- Done
type Num = Int
type Var = String
type Macro = String
type Prog = [Cmd]

data Mode = Up
          | Down
          deriving(Show)

data Expr = LitV Var
          | LitN Num
          | Add Expr Expr
          deriving(Show)

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving(Show)


-- | 2. Define a MiniLogo macro "line."
--
--      Concrete syntax in a comment:
--
--
--
--
--      Abstract syntax in code (include correct type header):
--

-- Done
line :: Cmd
line = Define "line" ["x1","y1","x2","y2"] [Pen Up, Move (LitV "x1", LitV "y1"), Pen Down, Move (LitV "x2", LitV "y2")]
-- Pen up move pen down move

-- | 3. Define a MiniLogo macro "nix" using "line" defined above.
--
--      Concrete syntax in a comment:
--
--      Pen Up;
--      Move (x1, y1);
--      Pen Down;
--      Move (x2, y2);
--
--
--      Abstract syntax in code (include correct type header):
--

-- Done
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"] [(Call "line" [(LitV "x"), (LitV "y"), (Add (LitV "x") (LitV "w")), (Add (LitV "y") (LitV "h"))]),
                                          (Call "line" [(LitV "x"), (Add (LitV "y") (LitV "h")), (Add (LitV "x") (LitV "w")), (LitV "y")])]


-- | 4. Define a Haskell function "steps" (steps :: Int -> Prog) that draws
--      a staircase of n steps starting from (0,0).
--

-- Done
steps :: Int -> Prog
steps 0 = []
steps n = steps (n - 1) ++ [Call "line" [ LitN (n - 1), LitN (n - 1), LitN (n - 1), LitN n ], Call "line" [ LitN (n - 1), LitN n, LitN n, LitN n ]]
--steps n = steps (n - 1) ++ Call "line" [ LitN n , LitN n , LitN n , LitN (n - 1) ] ++ Call "line" [ LitN n, LitN n, LitN (n - 1), LitN (n - 1) ]

-- | 5. Define a Haskell function "macros" (macros :: Prog -> [Macro] that
--      returns a list of the names of all the macros that are defined anywhere
--      in a given MiniLogo program.
--

-- Done
macros :: Prog -> [Macro]
macros [] = []
macros ((Define m _ p):mc) = m : macros p ++ macros mc
macros (_:mc) = macros mc

  -- Get a list of cmds
  -- Print all macros defined


-- | 6. Define a Haskell function "pretty" (pretty :: Prog -> String) that
--      "pretty-prints" a MiniLogo program.
--

-- Done
pretty :: Prog -> String
pretty [] = []
pretty (p:ps) = case p of
  Pen Up -> "pen up; \n" ++ pretty ps
  Pen Down -> "pen down; \n" ++ pretty ps
  Move (x, y) -> "move(" ++ prettyExpr x ++ prettyExpr y ++ "); \n " ++ pretty ps
  Call m es -> "call " ++ m ++ " (" ++ (intercalate "," (map prettyExpr es)) ++ "); \n " ++ pretty ps
  Define m v pr -> "define: " ++ m ++ " (" ++ (intercalate ", " v) ++ ") { \n " ++ pretty pr ++ "} \n " ++ pretty ps

prettyExpr :: Expr -> String
prettyExpr (LitV v) = v
prettyExpr (LitN n) = show n
prettyExpr (Add a b) = "add ( " ++ show a ++ " + " ++ show b ++ " )"




--
-- * Bonus Problems ----------------------------------------------------------
--
-- | 7. Define a Haskell function "optE" (optE :: Expr -> Expr) that partially
--      evaluates expressions by replacing additions of literals with the
--      result.
--

--
-- optE :: Expr -> Expr


-- | 8. Define a Haskell function "optP" (optP :: Prog -> Prog) that optimizes
--      all of the expressions contained in a given program using optE.
--

--
-- optP :: Prog -> Prog
