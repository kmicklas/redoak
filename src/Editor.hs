module Editor
  ( State
  , Event(..)
  , Key(..)
  , initState
  , onEvent
  ) where

import Data.Sequence

import Tree

data State
  = State
    { mode :: !Mode
    , currentId :: !Word
    , cursor :: (Tree Word Char, Path)
    }

data Mode
  = Normal
  | Insert
  deriving (Eq, Ord, Show)

data Event
  = KeyDown Key
  | KeyPress Char

data Key
  = ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Space
  | Enter
  | Other Int
  deriving (Eq, Ord, Show)

initState :: State
initState = State Normal 0 (empty, Path [] (0, 0))

onEvent :: Event -> State -> State

onEvent (KeyDown Enter) s = s { mode = Normal }
onEvent (KeyDown ArrowLeft)  s = s { cursor = shiftLeft  $ cursor s }
onEvent (KeyDown ArrowRight) s = s { cursor = shiftRight $ cursor s }
onEvent (KeyDown Space) s | mode s == Normal = s { mode = Insert }
onEvent (KeyDown _) s = s

onEvent (KeyPress '\n') s = s
onEvent (KeyPress c) s | mode s == Insert = s
  { cursor = change (singleton $ Atom (currentId s) c) $ cursor s
  , currentId = currentId s + 1
  }
onEvent _ s = s
