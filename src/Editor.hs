module Editor
  ( State(..)
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
    , cursor :: Cursor Word Char
    }
  deriving (Eq, Show)

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
  | Tab
  | Other Int
  deriving (Eq, Ord, Show)

initState :: State
initState = State Normal 0 $ Cursor empty $ Path [] (0, 0)

onEvent :: Event -> State -> State

onEvent (KeyDown ArrowLeft)  s = s { cursor = shiftLeft  $ cursor s }
onEvent (KeyDown ArrowRight) s = s { cursor = shiftRight $ cursor s }

onEvent (KeyDown Tab) s | mode s == Insert = s
  { cursor = push (currentId s) $ cursor s
  , currentId = currentId s + 1
  }
onEvent (KeyPress '\r') s | mode s == Insert = s
  { cursor = pop (currentId s) $ cursor s
  , currentId = currentId s + 1
  }
onEvent (KeyPress ' ') s | mode s == Insert = s
  { cursor = push (currentId s + 1) $ pop (currentId s) $ cursor s
  , currentId = currentId s + 2
  }

onEvent (KeyPress '\ESC') s = s
  { mode = case mode s of Normal -> Insert
                          Insert -> Normal
  }
onEvent (KeyPress c) s | mode s == Insert = s
  { cursor = selectNoneEnd $ change (singleton $ Atom (currentId s) c) $ cursor s
  , currentId = currentId s + 1
  }
onEvent (KeyDown _) s = s

onEvent _ s = s
