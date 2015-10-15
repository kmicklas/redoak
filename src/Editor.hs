module Editor
  (
  ) where

import Tree

data State
  = State
    { mode :: !Mode
    , tree :: Tree
    , cursor :: Path
    }

data Mode
  = Normal
  | Insert
  deriving (Eq, Ord, Show)

onChar :: Mode -> Char -> State -> State
onChar Insert 13 s = s { mode = Normal } -- enter
onChar Normal 37 s = moveLeft s
onChar Normal 38 s = moveUp s
onChar Normal 39 s = moveRight s
onChar Nomral 40 s = moveDown s
onChar Normal 32 s = s { mode = Insert } -- space
