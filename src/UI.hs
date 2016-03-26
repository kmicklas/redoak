{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module UI
  ( runEditor
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.List
import Data.Sequence hiding ((:<))
import Data.Sequences as SS
import Data.Text
import GHCJS.DOM (WebView)
import GHCJS.DOM.Document (Document, keyDown, keyPress, getBody)
import GHCJS.DOM.EventM (on, event, preventDefault, stopPropagation, uiKeyCode, uiCharCode)
import GHCJS.DOM.KeyboardEvent (getCtrlKey, getAltKey, getShiftKey)
import Reflex
import Reflex.Class
import Reflex.Dom
import Reflex.Dom.Widget.Basic

import Editor
import Event
import Layout
import Rectangle
import Tree
import View

viewState :: Editor -> View
viewState s = defaultLayout "editor" [] $ Node [contentView, statusView]
  where contentView = defaultLayout "content" [] $ Node [treeView]
        treeView = runIdentity $ layout $ cursor s
        statusView = defaultLayout "status" [] $ Node [pathView, modeView]
        pathView = defaultLayout "path" [] $ Atom $ pack $ pathString $ path $ cursor s
        modeView = defaultLayout "mode" [] $ Atom $ pack $ show $ mode s
        pathString (is, (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]

defaultLayout :: Text -> [Text] -> Element Text View -> View
defaultLayout id classes =
  (ViewInfo (Just id) classes (W 0, H 0) (X 0, Y 0) :<)

runEditor :: MonadWidget t m => Document -> m ()
runEditor doc = do
  keyDowns <- wrapDomEventMaybe doc (`on` keyPress) $ runMaybeT $ do
    k   <- MaybeT $ getKey <$> uiKeyCode
    e   <- lift $ event
    mod <- lift $ Modifiers <$> getCtrlKey e <*> getAltKey e <*> getShiftKey e
    return $ KeyDown k mod
  keyPresses <- wrapDomEvent doc (`on` keyDown) $ do
    KeyPress . toEnum <$> uiCharCode
  let keys = mergeWith (\l r -> l) [keyPresses, keyDowns]
  stateStream <- foldDyn handleEvent initState keys
  curState <- sample $ current stateStream
  makeNode $ viewState $ curState
