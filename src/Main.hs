module Main where

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)

import Editor
import Setup
import Tree
import UI
import View

main = runWebGUI $ \ webView -> do
  Just doc <- webViewGetDomDocument webView
  setup doc
  runEditor doc
