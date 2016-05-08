module Redoak.Language.DefaultInput where

import Control.Monad.Trans.State

import Redoak.Event
import Redoak.Language.Base
import Redoak.Language.Cursor


runStateOnly :: State s () -> s -> s
runStateOnly m = snd . runState m

basicTraversal :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
               => KeyEvent -> Maybe (EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ())
basicTraversal = \case
  KeyStroke Down ArrowLeft  (Modifiers _ _ False) -> Just $ tryEdit shiftLeft
  KeyStroke Down ArrowRight (Modifiers _ _ False) -> Just $ tryEdit shiftRight
  KeyStroke Down ArrowLeft  (Modifiers _ _ True)  -> Just $ tryEdit moveLeft
  KeyStroke Down ArrowRight (Modifiers _ _ True)  -> Just $ tryEdit moveRight
  KeyStroke Down ArrowUp    _ -> Just $ tryEdit ascend
  KeyStroke Down ArrowDown  _ -> Just $ tryEdit descend

  _ -> Nothing
