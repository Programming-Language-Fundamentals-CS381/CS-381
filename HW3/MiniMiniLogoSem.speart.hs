module MiniMiniLogoSem where

import MiniMiniLogo
import Render

--User Names
--Speart
--Warrickc
--Garcibru

--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--

-- Done
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen s) (m, p) = ((s, p), Nothing)
cmd (Move x1 y1) (s, (x2, y2)) = case s of
  Down -> ((s,(x1, y1)),Just ((x2,y2),(x1,y1)))
  Up -> ((s,(x1, y1)),Nothing)

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
--

--prog :: Prog -> State -> (State, [Line])
--prog [] s = (s, [])
--prog (h:t) s = case cmd h s of
--  (ns, Nothing) -> (fst(prog t ns), snd(prog t ns))
--  (ns, Just nl) -> (fst(prog t ns), nl : snd(prog t ns))

--prog :: Prog -> State -> (State, [Line])
--prog p s = (foldl changeState (s) p,foldl makeLineArray (s []) p)   --Base is wrong won't work

--changeState ::  State -> Cmd -> State --Looks Correct
--changeState s c = case cmd c s of
--  (ns, _) -> ns

--makeLineArray :: State -> [Line] -> Cmd -> [Line]   --State is wrong so won't work after Pen Down
--makeLineArray s l c = case cmd c s of
--  (ns, Nothing) -> l
--  (ns, Just nl) -> l ++ [nl]

-- Done
prog :: Prog -> State -> (State, [Line])
prog p s = foldl makelinearray (s, []) p    --Base stays the same from one move to another

makelinearray :: (State, [Line]) -> Cmd -> (State, [Line])  -- a -> b -> a
makelinearray (s, lin) c = case cmd c s of
  (ns, Nothing)   -> (ns, lin)
  (ns, Just nl) -> (ns, lin ++ [nl])

--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.

--Done 404
amazing :: Prog
--amazing = [ Move 395 0, Pen Down, Move 395 400, Move 396 400, Move 396 0, Move 397 0, Move 397 400, Move 398 400, Move 398 0, Move 399 0, Move 399 400, Move 400 400, Move 400 0, Move 401 0, Move 401 400, Move 402 400, Move 402 0, Move 403 0, Move 403 400, Move 404 400, Move 404 0, Move 405 0, Move 405 400 ]
amazing = [ Move 250 0, Pen Down, Move 250 400, Move 150 200, Move 250 200, Pen Up, Move 350 0, Pen Down, Move 450 0, Move 500 100, Move 500 300, Move 450 400, Move 350 400, Move 300 300, Move 300 100, Move 350 0, Pen Up, Move 650 0, Pen Down, Move 650 400, Move 550 200, Move 650 200 ]
