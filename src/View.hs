{-# LANGUAGE DeriveFunctor #-}

module View 
  ( Identified(..)
  , View(..)
  , reactView
  ) where

import Control.Concurrent.MVar
import Data.Text
import GHCJS.DOM.Document

import Dom
import React

data Identified i v
  = (:=) { ident :: i, value :: v }
  deriving Functor

data Element
  = Node { classes :: [Text], elements :: [View] }
  | Text { classes :: [Text], text :: Text }

type View = Identified Text Element

reactView :: Document -> MVar e -> s -> (e -> s -> s) -> (s -> View) -> IO ()
reactView document events init update render = do
  currentView <- newMVar $ Node [] []
  react events init update $ makeRenderer currentView
  where makeRenderer _ = effectView document . render

effectView :: Document -> View -> IO ()
effectView document new = do
  Just body <- getBody document
  return ()
