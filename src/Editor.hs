module Editor
  ( State(..)
  , Event(..)
  , Key(..)
  , initState
  , onEvent
  ) where

import Control.Monad
import Control.Monad.Trans.State.Lazy hiding (State)

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
  | Enter
  | Escape
  | Backspace
  | Delete
  | Other Int
  deriving (Eq, Ord, Show)

initState :: State
initState = State Normal 0 $ Cursor empty $ Path [] (0, 0)

applyEdit :: EditM Word Char -> (State -> State)
applyEdit f s = s { cursor = c'
                  , currentId = i'
                  }
  where (c', i') = runState (f $ cursor s) $ currentId s


onEvent :: Event -> State -> State

onEvent (KeyDown ArrowLeft)  s = s { cursor = shiftLeft  $ cursor s }
onEvent (KeyDown ArrowRight) s = s { cursor = shiftRight $ cursor s }

onEvent (KeyDown Tab) s | mode s == Insert = applyEdit push s
onEvent (KeyPress '\r') s | mode s == Insert = applyEdit pop s
onEvent (KeyPress ' ') s | mode s == Insert = applyEdit (pop <=< push) s

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
