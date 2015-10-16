module UI
  ( runEditor
  ) where

import GHCJS.DOM.Document (Document, keyDown, keyPress)
import GHCJS.DOM.EventM (on)
import Control.Concurrent.MVar

import Editor
import React

key :: Int -> Key
key 37 = ArrowLeft
key 38 = ArrowUp
key 39 = ArrowRight
key 40 = ArrowDown
key 32 = Space
key 13 = Enter
key c = Other c

runEditor :: Document -> IO a
runEditor doc = do
  putStrLn "run"
  events <- newEmptyMVar
  on doc keyDown $ do
    return ()
  on doc keyPress $ do
    return ()
  react events initState onEvent render
  where render = undefined
