{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module View 
  ( Element(..)
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
import Tree

type View = Element (Text, [Text]) Text

effectView :: View -> Dom ()
effectView new = do
  view <- makeNode new
  doc <- askDocument
  Just body <- liftIO $ getBody doc
  old <- getElementById doc (fst $ ident new)
  maybe (return ()) (removeNode . toNode) old
  appendChild body $ Just view
  return ()

removeNode :: Node -> Dom ()
removeNode node = do
  Just parent <- getParentNode node
  removeChild parent $ Just node
  return ()

makeNode :: View -> Dom Node
makeNode (Atom (id, cs) t) = el "span" (attrs id cs) [textNode t]
makeNode (Node (id, cs) es) = el "div" (attrs id cs) $ map makeNode $ toList es

attrs :: Text -> [Text] -> [(Text, Text)]
attrs id classes = [("id", id), ("class", intercalate " " classes)]
