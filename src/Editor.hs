{-# LANGUAGE OverloadedLists #-}

module Editor
  ( Editor(..)
  , initState
  , handleEvent
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Data.Maybe
import Data.Sequence
import Data.Text

import Event
import Tree

data Editor
  = Editor
    { mode :: !Mode
    , currentId :: !Word
    , cursor :: Cursor Text Word
    }
  deriving (Eq, Ord, Show)

data Mode
  = Normal
  | Insert
  deriving (Eq, Ord, Show)

initState :: Editor
initState = Editor Normal 1 $ T $ (0, Select (0, 0)) := Node []

apply :: EditT (State Word) Text Word () -> State Editor ()
apply e = do
  s <- get
  let (c', id') = runState (execStateT e $ cursor s) $ currentId s
  put $ s { currentId = id', cursor = c' }

inMode :: Mode -> State Editor () -> State Editor ()
inMode m a = do
  s <- get
  when (mode s == m) a

gotoMode :: Mode -> State Editor ()
gotoMode m = modify $ \ s -> s { mode = m }

handleEvent :: Event -> Editor -> Editor
handleEvent e = execState $ do
  onEvent e
  inMode Normal $ onEventNormal e
  inMode Insert $ onEventInsert e

onEvent :: Event -> State Editor ()
onEvent = \case

  KeyDown Tab   -> apply push

  KeyDown ArrowLeft  -> apply $ tryEdit shiftLeft
  KeyDown ArrowRight -> apply $ tryEdit shiftRight
  KeyDown ArrowUp    -> apply $ tryEdit ascend
  KeyDown ArrowDown  -> apply $ tryEdit descend

  _ -> return ()

onEventNormal :: Event -> State Editor ()
onEventNormal = \case

  KeyPress 'i' -> apply $ tryEdit ascend
  KeyPress 'k' -> apply $ tryEdit descend
  KeyPress 'j' -> apply $ tryEdit shiftLeft
  KeyPress 'l' -> apply $ tryEdit shiftRight
  KeyPress 'J' -> apply $ tryEdit moveLeft
  KeyPress 'L' -> apply $ tryEdit moveRight

  KeyPress 'a'  -> apply $ tryEdit selectAll
  KeyPress 's'  -> apply $ tryEdit switchBounds
  KeyPress 'd'  -> apply delete
  KeyPress 'f'  -> apply $ tryEdit selectNoneEnd

  KeyPress ' '  -> gotoMode Insert

  _ -> return ()

onEventInsert :: Event -> State Editor ()
onEventInsert = \case

  KeyDown Enter -> gotoMode Normal
  KeyPress ' '  -> apply $ tryEdit pop

  KeyPress c ->
    apply $ tryEdit $ justEdit (change $ Atom [c])
                   >> justEdit (tryEdit $ descend >> endMax)
                   >> selectNoneEnd

  _ -> return ()
