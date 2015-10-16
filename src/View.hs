{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module View 
  ( Identified(..)
  , View(..)
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text hiding (map)
import Data.Sequence
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Node (Node, appendChild)

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
  Just body <- askDocument >>= liftIO . getBody
  return ()

makeNode :: View -> Dom Node
makeNode (id := Text cs t) = el "span" (attrs id cs) [textNode t]
makeNode (id := Node cs es) = el "div" (attrs id cs) $ map makeNode $ toList es

attrs :: Text -> [Text] -> [(Text, Text)]
attrs id classes = [("id", id), ("class", intercalate " " classes)]
