{-# LANGUAGE OverloadedStrings #-}

module UI
  ( runEditor
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Text
import GHCJS.DOM.Document (Document, keyDown, keyPress)
import GHCJS.DOM.EventM (on, uiKeyCode, uiCharCode)

import Dom
import Editor
import React
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
viewState s = Atom ("content", []) $ pack $ show s

runEditor :: Document -> IO ()
runEditor doc = do
  events <- newEmptyMVar
  on doc keyDown $ do
    code <- uiKeyCode
    liftIO $ putMVar events $ KeyDown $ key code
  on doc keyPress $ do
    code <- uiCharCode
    liftIO $ putMVar events $ KeyPress $ toEnum code
  forkIO $ react events initState onEvent render
  return ()
  where render s = run (effectView $ viewState s) doc
