{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import Data.Bifunctor
import Data.Foldable
import Data.Sequence
import Data.Text (Text, pack)
import Data.Traversable
import Prelude   hiding (foldr)

import Rectangle
import Tree
import View

-- MAGIC CONSTANTS:
indentWidth = W 50
maxInlineHeight = H 50
inlinePad = W 20

data LayoutInfo
  = LayoutInfo
    { ident :: Maybe Text
    , selection :: Maybe Range
    --, alwaysBreak :: Bool
    --, headed :: Bool -- is first child special
    --, group :: Bool  -- is this an element from the original tree
    }

type Layout    = Tree Text LayoutInfo
type LayoutDim = Tree Text (LayoutInfo, Dimensions)

data Direction
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

class (Monad r) => Rules r where
  inlineText :: Text -> r Dimensions
  layoutText :: Width -> Text -> r Dimensions

makeLayout :: Cursor Text Word -> Layout
makeLayout = layoutWithSelection . findPath True
  where layoutWithSelection :: Tree Text ((Word, Selection), Bool) -> Layout
        layoutWithSelection = second $ \ ((id, sel), onPath) -> LayoutInfo
          { Layout.ident = Just $ pack $ show id
          , selection = case (onPath, sel) of
              (True, Select r) -> Just r
              _                -> Nothing
          }

        findPath :: Bool
                 -> Cursor Text Word
                 -> Tree Text ((Word, Selection), Bool)
        findPath onPath (T (a@(_, sel) := e)) = T $ ((a, onPath) :=) $ case e of
          Atom a  -> Atom a
          Node ts -> Node $ case sel of
            Select  _ -> findPath False <$> ts
            Descend i ->
              fmap (uncurry findPath)
                $ fmap (first (== i))
                $ snd
                $ mapAccumL (\ count elem -> (count + 1, (count, elem))) 0 ts

computeFull :: Rules r => Layout -> r LayoutDim
computeFull (T (info := e)) = do
    (dim, e') <- case e of
      Atom a -> do
        (, Atom a) <$> inlineText a
      Node ts -> do
        fulls <- mapM computeFull ts
        let fullDims = fmap (snd . ann . unTree) fulls
        let maxWidth  = maximum $ fmap fst fullDims
        let maxHeight = maximum $ fmap snd fullDims
        let dim = if maxHeight <= maxInlineHeight
                  then (sum $ fmap ((+ inlinePad) . fst) fullDims, maxHeight)
                  else (maxWidth, sum $ fmap snd fullDims)
        return (dim, Node fulls)
    return $ T $ (info, dim) := e'

-- all nodes must have full size computed
layoutFull :: Width -> LayoutDim -> View
layoutFull mw t@(T ((info, (w, h)) := e)) =
  if w <= mw
  then second (makeViewInfo Horizontal) t
  else case e of
    Atom a -> second (makeViewInfo Vertical) t
    Node ts ->
      case viewl ts of
        EmptyL -> second (makeViewInfo Vertical) t
        first :< rest ->
          let views = layoutFull mw first
                   <| fmap (layoutFull (mw - indentWidth)) rest in
          let fullDim = ( maximum $ fmap (fst . dim . ann . unTree) views
                        , sum $ fmap (snd . dim . ann . unTree) views
                        ) in
          T $ makeViewInfo Horizontal (info, fullDim) := Node views

dirClass :: Direction -> Text
dirClass Horizontal = "horizontal"
dirClass Vertical   = "vertical"

makeViewInfo :: Direction -> (LayoutInfo, Dimensions) -> ViewInfo
makeViewInfo dir (LayoutInfo id sel, dim) = ViewInfo
  { View.ident = id
  , classes = ["content", dirClass dir]
  , dim = dim
  , pos = (X 0, Y 0)
  }
