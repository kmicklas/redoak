module Event
  ( Event(..)
  , Key(..)
  , Modifiers(..)
  , getKey
  ) where

data Event
  = KeyDown Key Modifiers
  | KeyPress Char
  deriving (Eq, Ord, Show)

data Key
  = ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Tab
  | Space
  | Enter
  | Escape
  | Backspace
  | Delete
  deriving (Eq, Ord, Show)

data Modifiers
  = Modifiers { control :: Bool, alt :: Bool, shift :: Bool }
  deriving (Eq, Ord, Show)

getKey :: Int -> Maybe Key
getKey = \case
  37 -> Just ArrowLeft
  38 -> Just ArrowUp
  39 -> Just ArrowRight
  40 -> Just ArrowDown
  9  -> Just Tab
  32 -> Just Space
  13 -> Just Enter
  27 -> Just Escape
  8  -> Just Backspace
  46 -> Just Delete
  _  -> Nothing
