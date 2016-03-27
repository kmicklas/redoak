{-# LANGUAGE ImplicitParams #-}

module Main where

import Editor
import Setup
import Tree
import UI
import View

import GHCJS.DOM.Document (getBody)
import Reflex.Dom

main :: IO ()
main = withDocument $ \ webView doc -> do
  Just body <- getBody doc
  attachWidget body webView $ runEditor doc
