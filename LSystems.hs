module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)
  
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (x, _, _) = x

-- |Returns the base string for the given system.
base :: System -> String
base (_, x, _) = x

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, x) = x


-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar c rules = concat [ y | (x, y) <- rules, x == c ]

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rules base = concat [ lookupChar x rules | x <- base ]

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules base 0 = base
expand rules base n = expand rules (expandOne rules base) (n-1)

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move action ((x, y), angle) dir
  | action == 'F' = ((newX, newY), angle)
  | otherwise = ((x, y), newAngle)
  where
  angleRad = (pi * angle) / 180
  (newX, newY) = (x + cos angleRad, y + sin angleRad)
  newAngle = if action == 'L' then angle + dir else angle - dir

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 cmds rotation colour
  = trace1' (0, 0) 90 cmds rotation
  where
    trace1FindBracket (x : xs) count
      | x == '[' = trace1FindBracket xs (count+1)
      | x == ']' =
        if count == 0
        then xs
        else trace1FindBracket xs (count-1)
      | otherwise = trace1FindBracket xs count
    trace1' _ _ [] _ = []
    trace1' (x, y) angle (currentCmd : remCmds) rotation
      | currentCmd == ']' = []
      | currentCmd == '[' = (trace1' (x, y) angle remCmds rotation) ++ (trace1' (x, y) resetAngle (trace1FindBracket remCmds 0) rotation)
      | currentCmd == 'F' = ((x, y), (newX, newY), colour) : (trace1' (newX, newY) newAngle remCmds rotation)
      | otherwise = trace1' (newX, newY) newAngle remCmds rotation
      where
        resetAngle = 90
        ((newX, newY), newAngle) = move currentCmd ((x, y), angle) rotation

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 cmds rotation colour
  = trace2' (0, 0) 90 cmds rotation [(0, 0)]
  where
    trace2' _ _ [] _ _ = []
    trace2' (x, y) angle (currentCmd : remCmds) rotation states@((xState, yState) : remStates)
      | currentCmd == '[' = trace2' (x, y) angle remCmds rotation ((x, y) : states)
      | currentCmd == ']' = trace2' (xState, yState) resetAngle remCmds rotation remStates
      | currentCmd == 'F' = ((x, y), (newX, newY), colour) : (trace2' (newX, newY) newAngle remCmds rotation states)
      | otherwise = trace2' (newX, newY) newAngle remCmds rotation states
      where
        resetAngle = 90
        ((newX, newY), newAngle) = move currentCmd ((x, y), angle) rotation


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
