{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module View 
  ( Identified(..)
  , Element(..)
  , View
  , effectView
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text hiding (map)
import Data.Sequence
import GHCJS.DOM.Document (getBody, getElementById)
import GHCJS.DOM.Node (Node, toNode, appendChild, getParentNode, removeChild)

import Dom
import React

data Identified i v
  = (:=) { ident :: i, value :: v }
  deriving Functor

data Element
  = Node { classes :: [Text], elements :: Seq View }
  | Text { classes :: [Text], text :: Text }

type View = Identified Text Element

effectView :: View -> Dom ()
effectView new = do
  view <- makeNode new
  doc <- askDocument
  Just body <- liftIO $ getBody doc
  getElementById doc (ident new) >>= (maybe (return ()) $ removeNode . toNode)
  appendChild body $ Just view
  return ()

removeNode :: Node -> Dom ()
removeNode node = do
  Just parent <- getParentNode node
  removeChild parent $ Just node
  return ()

makeNode :: View -> Dom Node
makeNode (id := Text cs t) = el "span" (attrs id cs) [textNode t]
makeNode (id := Node cs es) = el "div" (attrs id cs) $ map makeNode $ toList es

attrs :: Text -> [Text] -> [(Text, Text)]
attrs id classes = [("id", id), ("class", intercalate " " classes)]
