{-# LANGUAGE TupleSections #-}
module Redoak.Event where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty
import GHCJS.DOM.Document
import GHCJS.DOM.EventM
import GHCJS.DOM.KeyboardEvent
import qualified Reflex.Dom as RD


data KeyStroke
  = Up
  | Down
  deriving (Eq, Ord, Show)


data KeyEvent
  = KeyStroke KeyStroke Key Modifiers
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


globalKeyEvents :: forall t m. RD.MonadWidget t m
                => Document
                -> m (RD.Event t (NonEmpty KeyEvent))
globalKeyEvents doc = RD.mergeList <$> sequence
  [ RD.wrapDomEventMaybe doc (`on` keyDown)  $ bigFatHandler Down
  , RD.wrapDomEventMaybe doc (`on` keyUp)    $ bigFatHandler Up
  , RD.wrapDomEvent doc (`on` keyPress) $ KeyPress . toEnum <$> uiCharCode
  ]
  where bigFatHandler t = runMaybeT $ do
          k    <- MaybeT $ getKey <$> uiKeyCode
          e    <- lift $ event
          mods <- lift $ Modifiers <$> getCtrlKey e <*> getAltKey e <*> getShiftKey e
          lift $ do
            stopPropagation
            preventDefault
          return $ KeyStroke t k mods
