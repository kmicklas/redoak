{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Editor
  ( Editor(..)
  , initState
  , handleEvent
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Data.Bifunctor
import Data.Maybe
import Data.Sequence hiding ((:<))
import Data.Sequences (IsSequence)
import Data.Text hiding (copy)

import Event
import Tree

data Editor
  = Editor
    { mode :: !Mode
    , currentId :: !Word
    , cursor :: Cursor Text Word
    , clipboard :: Trunk Text ()
    }
  deriving (Eq, Ord, Show)

data Mode
  = Normal
  | Insert
  deriving (Eq, Ord, Show)

initState :: Editor
initState = Editor
  { mode = Normal
  , currentId = 1
  , cursor = (0, Select (0, 0)) :< Node []
  , clipboard = Node []
  }

apply :: EditT (State Word) Text Word a -> State Editor a
apply e = do
  s <- get
  let ((r, c'), id') = runState (runStateT e $ cursor s) $ currentId s
  put $ s { currentId = id', cursor = c' }
  return r

inMode :: Mode -> State Editor () -> State Editor ()
inMode m a = do
  s <- get
  when (mode s == m) a

gotoMode :: Mode -> State Editor ()
gotoMode m = modify $ \ s -> s { mode = m }

copy :: State Editor ()
copy = do
  s <- get
  sel <- fmap clearAnn <$> apply getSelection
  put $ s { clipboard = sel }

paste :: State Editor ()
paste = do
  s <- get
  new <- apply $ lift $ mapM initAnn $ clipboard s
  apply $ change $ second initCursor $ new

deleteBackward :: (IsSequence a, Fresh ann, Monad m)
               => MaybeEditT (StateT ann m) a ann ()
deleteBackward = do
  e <- isEmpty
  if e
  then moveLeft >> justEdit delete
  else justEdit delete

deleteForward :: (IsSequence a, Fresh ann, Monad m)
              => MaybeEditT (StateT ann m) a ann ()
deleteForward = do
  e <- isEmpty
  if e
  then moveRight >> justEdit delete
  else justEdit delete

pushNode :: (IsSequence a, Fresh ann, Monad m)
            => MaybeEditT (StateT ann m) a ann ()
pushNode = do
  a <- isInAtom
  if a
  then pop >> justEdit push
  else justEdit push

handleEvent :: Event -> Editor -> Editor
handleEvent e = execState $ do
  onEvent e
  inMode Insert $ onEventInsert e
  inMode Normal $ onEventNormal e

onEvent :: Event -> State Editor ()
onEvent = \case

  KeyDown Tab   -> apply $ tryEdit pushNode

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

  KeyPress 'a' -> apply $ tryEdit selectAll
  KeyPress 's' -> apply $ tryEdit switchBounds
  KeyPress 'd' -> apply delete
  KeyPress 'f' -> apply $ tryEdit selectNoneEnd

  KeyDown Backspace -> apply $ tryEdit deleteBackward
  KeyDown Delete    -> apply $ tryEdit deleteForward

  KeyPress 'c' -> copy
  KeyPress 'x' -> copy >> apply delete
  KeyPress 'v' -> paste

  KeyPress 'n' -> apply insertNode
  KeyPress 'r' -> apply Tree.reverse

  KeyPress 'w' -> apply wrap
  KeyPress 'e' -> apply $ tryEdit unwrap

  KeyPress 'h' -> gotoMode Insert

  _ -> return ()

onEventInsert :: Event -> State Editor ()
onEventInsert = \case

  KeyDown Enter -> gotoMode Normal
  KeyPress ' '  -> apply $ tryEdit pop

  KeyDown Backspace -> apply $ tryEdit deleteBackward
  KeyDown Delete    -> apply $ tryEdit deleteForward

  KeyPress c ->
    apply $ tryEdit $ justEdit (change $ Atom [c])
                   >> justEdit (tryEdit $ descend >> endMax)
                   >> selectNoneEnd

  _ -> return ()
