{-# LANGUAGE OverloadedLists #-}

module Editor
  ( State(..)
  , initState
  , onEvent
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy hiding (State)

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
initState = State Normal 1 (T $ (0, Select (0, 0)) := Node []) []

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

onEvent' (KeyDown ArrowLeft)  s = applyFailableEdit s shiftLeft
onEvent' (KeyDown ArrowRight) s = applyFailableEdit s shiftRight

onEvent' (KeyPress 'i') s | mode s == Normal = applyFailableEdit s ascend
onEvent' (KeyPress 'k') s | mode s == Normal = applyFailableEdit s descend
onEvent' (KeyPress 'j') s | mode s == Normal = applyFailableEdit s shiftLeft
onEvent' (KeyPress 'l') s | mode s == Normal = applyFailableEdit s shiftRight
onEvent' (KeyPress 'J') s | mode s == Normal = applyFailableEdit s moveLeft
onEvent' (KeyPress 'L') s | mode s == Normal = applyFailableEdit s moveRight

onEvent' (KeyPress 'd') s | mode s == Normal = applyEdit s $ change $ Node []

onEvent' (KeyDown Tab)   s | mode s == Insert = applyEdit s push
onEvent' (KeyPress ' ')  s | mode s == Insert = applyFailableEdit s pop

onEvent' (KeyPress ' ')  s | mode s == Normal = s { mode = Insert }
onEvent' (KeyDown Enter) s | mode s == Insert = s { mode = Normal }

onEvent' (KeyPress c) s | mode s == Insert =
  applyFailableEdit s $ justEdit (change $ Atom [c])
                    >=> justEdit (maybeEdit $ descend >=> endMax)
                    >=> selectNoneEnd

onEvent' _ s = s
