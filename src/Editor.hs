{-# LANGUAGE OverloadedLists #-}

module Editor
  ( State(..)
  , initState
  , onEvent
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy hiding (State)
--import Control.Monad.Trans.State.Lazy hiding (State)

import Data.Maybe
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

applyFailableEdit :: State -> EditT Maybe Text Word -> State
applyFailableEdit s f = fromMaybe s $ do
  (c', i') <- runStateT (f $ cursor s) $ currentId s
  return $ s { cursor = c'
             , currentId = i'
             }

onEvent :: Event -> State -> State
onEvent e s = onEvent' e $ s { events = e : events s }

onEvent' :: Event -> State -> State

onEvent' (KeyDown ArrowLeft)  s = applyEdit s shiftLeft
onEvent' (KeyDown ArrowRight) s = applyEdit s shiftRight

onEvent' (KeyDown Tab)   s | mode s == Insert = applyEdit s push
onEvent' (KeyDown Enter) s | mode s == Insert = applyFailableEdit s pop
onEvent' (KeyPress ' ')  s | mode s == Insert = applyFailableEdit s $
                                                pop >=> mapEdit (Just . runIdentity) push

onEvent' (KeyDown Escape) s = s
  { mode = case mode s of Normal -> Insert
                          Insert -> Normal
  }
onEvent' (KeyPress c) s | mode s == Insert = applyEdit s $ change (Atom [c]) >=> selectNoneEnd

onEvent' _ s = s
