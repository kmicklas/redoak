{-# LANGUAGE ImplicitParams #-}

module Main where

import GHCJS.DOM.Document (getBody)
import Reflex.Dom

import Redoak
import Redoak.Editor
import Redoak.Event
import Redoak.Setup
import Redoak.View


main :: IO ()
main = withDocument $ \ webView doc -> do
  Just body <- getBody doc
  attachWidget body webView $ editor =<< globalKeyEvents doc
