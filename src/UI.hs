{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI
  ( runEditor
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Sequence
import Data.Text
import GHCJS.DOM.Document (Document, keyDown, keyPress)
import GHCJS.DOM.EventM (on, preventDefault, stopPropagation, uiKeyCode, uiCharCode)

import Dom
import Editor
import React
import Tree
import View

key :: Int -> Key
key = \case
  37 -> ArrowLeft
  38 -> ArrowUp
  39 -> ArrowRight
  40 -> ArrowDown
  9  -> Tab
  13 -> Enter
  27 -> Escape
  8  -> Backspace
  46 -> Delete
  c -> Other c

isSpecial :: Key -> Bool
isSpecial (Other _) = False
isSpecial _         = True

viewState :: State -> View
viewState s = Node ("editor", []) $ fromList [contentView, modeView, debugView]
  where contentView = Node ("content", []) treeViews
        treeViews = viewTree ["content"] $ stringify $ tree $ cursor s
        modeView = Atom ("mode", []) $ pack $ show $ mode s
        debugView = Atom ("debug", []) $ pack $ show s

viewTree :: forall i. (Show i) => [Text] -> Tree i Text -> Seq View
viewTree classes = outer
  where
    outer = fmap viewElement

    viewElement :: Element i Text -> View
    viewElement = \case
      (Atom i a)  -> Atom (pack $ show i, classes) a
      (Node i ts) -> Node (pack $ show i, classes) $ outer ts

runEditor :: WithDoc ()
runEditor = do
  events <- newEmptyMVar
  on ?doc keyDown $ do
    code <- fmap key uiKeyCode
    when (isSpecial code) $ do
      stopPropagation
      preventDefault
    liftIO $ putMVar events $ KeyDown code
  on ?doc keyPress $ do
    code <- uiCharCode
    liftIO $ putMVar events $ KeyPress $ toEnum code
  forkIO $ react events onEvent (effectView . viewState) initState
  return ()
