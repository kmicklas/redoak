{-# LANGUAGE OverloadedLists #-}

module Editor
  ( State(..)
  , initState
  , onEvent
  ) where

import Control.Monad
import Control.Monad.Trans.State.Lazy hiding (State)

import Data.Sequence
import Data.Text

import Event
import Tree

data State
  = State
    { mode :: !Mode
    , currentId :: !Word
    , cursor :: Cursor Text Word
    , events :: [Event]
    }
  deriving (Eq, Ord, Show)

data Mode
  = Normal
  | Insert
  deriving (Eq, Ord, Show)

initState :: State
initState = State Normal 1 (Cursor (T $ 0 := Node []) $ Select (0, 0)) []

applyEdit :: State -> EditM Text Word -> State
applyEdit s f = s { cursor = c'
                  , currentId = i'
                  }
  where (c', i') = runState (f $ cursor s) $ currentId s

onEvent :: Event -> State -> State
onEvent e s = onEvent' e $ s { events = e : events s }

onEvent' :: Event -> State -> State

onEvent' (KeyDown ArrowLeft)  s = applyEdit s shiftLeft
onEvent' (KeyDown ArrowRight) s = applyEdit s shiftRight

onEvent' (KeyDown Tab)   s | mode s == Insert = applyEdit s push
onEvent' (KeyDown Enter) s | mode s == Insert = applyEdit s pop
onEvent' (KeyPress ' ')  s | mode s == Insert = applyEdit s $ pop >=> push

onEvent' (KeyDown Escape) s = s
  { mode = case mode s of Normal -> Insert
                          Insert -> Normal
  }
onEvent' (KeyPress c) s | mode s == Insert = applyEdit s $ change (Atom [c]) >=> selectNoneEnd

onEvent' _ s = s
