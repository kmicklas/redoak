{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI
  ( runEditor
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Sequence
import Data.Text
import GHCJS.DOM.Document (Document, keyDown, keyPress)
import GHCJS.DOM.EventM (on, uiKeyCode, uiCharCode)

import Dom
import Editor
import React
import Tree
import View

key :: Int -> Key
key 37 = ArrowLeft
key 38 = ArrowUp
key 39 = ArrowRight
key 40 = ArrowDown
key 32 = Space
key 13 = Enter
key c = Other c

viewState :: State -> View
viewState s = Node ("editor", []) $ fromList [contentView, modeView]
  where contentView = Node ("content", []) treeViews
        treeViews = viewTree $ fmap (fmap pack) $ stringify $ tree $ cursor s
        modeView = Atom ("mode", []) $ pack $ show $ mode s

viewTree :: (Show i) => Tree i Text -> Seq View
viewTree = fmap viewElement

viewElement :: (Show i) => Element i Text -> View
viewElement (Atom i a) = Atom (pack $ show i, []) a
viewElement (Node i ts) = Node (pack $ show i, []) $ viewTree ts

runEditor :: WithDoc ()
runEditor = do
  events <- newEmptyMVar
  on ?doc keyDown $ do
    code <- uiKeyCode
    liftIO $ putMVar events $ KeyDown $ key code
  on ?doc keyPress $ do
    code <- uiCharCode
    liftIO $ putMVar events $ KeyPress $ toEnum code
  forkIO $ react events initState onEvent $ effectView . viewState
  return ()
