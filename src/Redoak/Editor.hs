{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Editor
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

import Redoak.Event
import Redoak.Language
import Redoak.Language.Fundamental


data Editor
  = Editor
    { mode :: !Mode
    , currentId :: !Word
    , cursor :: Cursor' Text Word
    , clipboard :: Trunk Text ()
    }
  --deriving (Eq, Ord, Show)

data Mode
  = Normal
  | Insert
  deriving (Eq, Ord, Show)

initState :: Editor
initState = Editor
  { mode = Normal
  , currentId = 1
  , cursor = (0, Select $ Range (0, 0)) :< Node []
  , clipboard = Node []
  }

apply :: EditT' (State Word) Text Word a -> State Editor a
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
               => MaybeEditT' (StateT ann m) a ann ()
deleteBackward = do
  e <- isEmpty
  if e
  then moveLeft >> justEdit delete
  else justEdit delete

deleteForward :: (IsSequence a, Fresh ann, Monad m)
              => MaybeEditT' (StateT ann m) a ann ()
deleteForward = do
  e <- isEmpty
  if e
  then moveRight >> justEdit delete
  else justEdit delete

pushNode :: (IsSequence a, Fresh ann, Monad m)
            => MaybeEditT' (StateT ann m) a ann ()
pushNode = do
  a <- isInAtom
  if a
  then pop >> justEdit push
  else justEdit push

selectOne :: (IsSequence a, Monad m) => MaybeEditT' m a ann ()
selectOne = selectNoneEnd >> maybeEdit moveRight (moveLeft >> switchBounds)

insert :: (IsSequence a, Fresh ann, Monad m)
       => a -> MaybeEditT' (StateT ann m) a ann ()
insert text = do
  justEdit (change $ Atom text)
  justEdit (tryEdit $ descend >> endMax)
  selectNoneEnd

handleEvent :: KeyEvent -> Editor -> Editor
handleEvent e = execState $ do
  onEvent e
  get >>= return . mode >>= \case
    Insert -> onEventInsert e
    Normal -> onEventNormal e

onEvent :: KeyEvent -> State Editor ()
onEvent = \case

  KeyStroke Down Tab (Modifiers _ _ False) -> apply $ tryEdit pushNode

  KeyStroke Down ArrowLeft  (Modifiers _ _ False) -> apply $ tryEdit shiftLeft
  KeyStroke Down ArrowRight (Modifiers _ _ False) -> apply $ tryEdit shiftRight
  KeyStroke Down ArrowLeft  (Modifiers _ _ True)  -> apply $ tryEdit moveLeft
  KeyStroke Down ArrowRight (Modifiers _ _ True)  -> apply $ tryEdit moveRight
  KeyStroke Down ArrowUp    _ -> apply $ tryEdit ascend
  KeyStroke Down ArrowDown  _ -> apply $ tryEdit descend

  KeyStroke Down Backspace _ -> apply $ tryEdit deleteBackward
  KeyStroke Down Delete    _ -> apply $ tryEdit deleteForward

  _ -> return ()

onEventNormal :: KeyEvent -> State Editor ()
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
  KeyPress 'g' -> apply $ tryEdit selectOne

  KeyPress 'c' -> copy
  KeyPress 'x' -> copy >> apply delete
  KeyPress 'v' -> paste

  KeyPress 'n' -> apply insertNode
  KeyPress 'r' -> apply Redoak.Language.Fundamental.reverse

  KeyPress 'w' -> apply wrap
  KeyPress 'e' -> apply $ tryEdit unwrap

  KeyPress 'h' -> gotoMode Insert

  _ -> return ()

onEventInsert :: KeyEvent -> State Editor ()
onEventInsert = \case

  KeyStroke Down Enter (Modifiers _ _ False) -> gotoMode Normal
  KeyStroke Down Space (Modifiers _ _ False) -> apply $ tryEdit pop

  KeyStroke Down Enter (Modifiers _ _ True) -> apply $ tryEdit $ insert "\n"
  KeyStroke Down Space (Modifiers _ _ True) -> apply $ tryEdit $ insert " "
  KeyStroke Down Tab   (Modifiers _ _ True) -> apply $ tryEdit $ insert "\t"

  KeyPress c -> apply $ tryEdit $ insert [c]
  _ -> return ()
