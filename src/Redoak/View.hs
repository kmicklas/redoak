{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Redoak.View
  ( ViewInfo(..)
  , View
  , makeNode
  ) where

import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom

import           Redoak.Rectangle
import           Redoak.Languages.Fundamental


data ViewInfo n
  = ViewInfo
    { ident :: Maybe Text
    , classes :: [Text]
    , dim :: Dimensions n
    , pos :: Position n
    }
  deriving (Eq, Ord, Show)

type View n = Tree Text (ViewInfo n)


makeNode :: MonadWidget t m => View n -> m ()
makeNode (ViewInfo id cs _ _ :< e) = elAttr typ as $ case e of
    Atom t  -> text t
    Node es -> mapM_ makeNode es
  where typ = case e of
          Atom t  -> "span"
          Node es -> "div"
        as = attrs id cs

attrs :: Maybe Text -> [Text] -> AttributeMap
attrs id classes = M.fromList $
  [("class", T.intercalate " " classes)] ++ maybe [] (return . ("id",)) id
