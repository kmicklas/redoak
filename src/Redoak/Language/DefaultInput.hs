module Redoak.Language.DefaultInput where

import Control.Monad.Trans.State

import Redoak.Event
import Redoak.Language.Base
import Redoak.Language.Cursor


runStateOnly :: State s () -> s -> s
runStateOnly m = snd . runState m

basicTraversal :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
               => KeyEvent -> Maybe (EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ())
basicTraversal = fmap tryEdit . \case
    KeyStroke Down ArrowLeft  (Modifiers False _ False)  -> Just shiftLeft
    KeyStroke Down ArrowRight (Modifiers False _ False)  -> Just shiftRight
    KeyStroke Down ArrowUp    (Modifiers _     _ _)      -> Just ascend
    KeyStroke Down ArrowDown  (Modifiers _     _ _)      -> Just descend

    KeyStroke Down ArrowLeft  (Modifiers False _ True)   -> Just moveLeft
    KeyStroke Down ArrowRight (Modifiers False _ True)   -> Just moveRight

    KeyStroke Down ArrowLeft  (Modifiers True  _ False)  -> Just $ leafMove Leftwards
    KeyStroke Down ArrowRight (Modifiers True  _ False)  -> Just $ leafMove Rightwards


    KeyStroke Down ArrowLeft  (Modifiers True  _ True)   -> Just $ descendAll SelectNone Leftwards
    KeyStroke Down ArrowRight (Modifiers True  _ True)   -> Just $ descendAll SelectNone Rightwards

    _ -> Nothing
