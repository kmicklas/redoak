{-# LANGUAGE DeriveFunctor #-}

module View 
  ( Identified(..)
  , View(..)
  , reactView
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
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

reactView :: MVar e -> s -> (e -> s -> s) -> (s -> View) -> Dom ()
reactView events init update render = do
  currentView <- liftIO $ newMVar $ Node [] []
  react events init update $ effectView . render

effectView :: View -> Dom ()
effectView new = do
  Just body <- askDocument >>= liftIO . getBody
  return ()

--makeNode :: View -> Dom Node
--makeNode (id := Node cs es) = el "div" 
