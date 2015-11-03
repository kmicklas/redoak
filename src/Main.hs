module Main where

import Data.Maybe
import GHCJS.DOM (webViewGetDomDocument, runWebGUI)

import Editor
import Setup
import Tree
import UI
import View

main = runWebGUI $ \ view -> do
  setup view
  webViewGetDomDocument view >>= (runEditor . fromJust)
