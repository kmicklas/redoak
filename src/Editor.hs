module Editor
  (
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

data Key
  = ArrowLeft | ArrowRight | ArrowUp | ArrowDown
  | Space
  | Enter
  | Other Int
  deriving (Eq, Ord, Show)

key :: Int -> Key
key 37 = ArrowLeft
key 38 = ArrowUp
key 39 = ArrowRight
key 40 = ArrowDown
key 32 = Space
key 13 = Enter
key c = Other c

onKey :: Key -> State -> State
onKey Enter s = s { mode = Normal }
onKey ArrowLeft  s = s { cursor = shiftLeft  $ cursor s }
onKey ArrowRight s = s { cursor = shiftRight $ cursor s }
onKey Space s | mode s == Normal = s { mode = Insert }
onKey _ s = s

onChar :: Char -> State -> State
onChar '\n' s = s
onChar c s | mode s == Insert = s
  { cursor = change (singleton $ Atom (currentId s) c) $ cursor s
  , currentId = currentId s + 1
  }
