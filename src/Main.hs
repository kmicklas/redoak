{-# LANGUAGE ImplicitParams #-}

module Main where

import GHCJS.DOM.Document (getBody)
import Reflex.Dom

import Redoak.Editor
import Redoak.Setup
import Redoak.Tree
import Redoak.UI
import Redoak.View


main :: IO ()
main = withDocument $ \ webView doc -> do
  Just body <- getBody doc
  attachWidget body webView $ runEditor doc
