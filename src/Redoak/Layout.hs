{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Layout where

import           Control.Comonad
import           Control.Monad
import           Control.Lens hiding ((:<), (<|))
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Foldable
import           Data.Sequence hiding ((:<))
import qualified Data.Sequence as S
import           Data.Sequences as SS
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Traversable
import           Prelude hiding (foldr)

import           Control.Comonad.Cofree8

import           Redoak.Rectangle
import           Redoak.Language.Fundamental
import           Redoak.Tree.Range
import           Redoak.View


-- MAGIC CONSTANTS:
data LayoutInfo
  = LayoutInfo
    { ident :: Word
    , selection :: Maybe (Range Word)
    --, alwaysBreak :: Bool
    --, headed :: Bool -- is first child special
    --, group :: Bool  -- is this an element from the original tree
    }
  deriving (Eq, Ord, Show)

type Layout       = Tree Text                 LayoutInfo
type LayoutAtom n = Tree (Text, Dimensions n) (LayoutInfo)
type LayoutDim n  = Tree Text                 (LayoutInfo, Dimensions n)

data Direction
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

class (Adequate (BaseNum r), Monad r) => Rules r where
  type BaseNum r :: *
  inlineText :: Text -> r (Dimensions (BaseNum r))

-- TOOD based on monad
class (Integral n, Num n, Ord n, Read n, Real n, Show n) => Adequate n where
  indentWidth     :: Width  n -- TODO explicit type app
  maxInlineHeight :: Height n
  inlinePad       :: Width  n

type Width'      r = Width      (BaseNum r)
type Height'     r = Height     (BaseNum r)
type X'          r = X          (BaseNum r)
type Y'          r = Y          (BaseNum r)
type Dimensions' r = Dimensions (BaseNum r)
type Position'   r = Position   (BaseNum r)
type View'       r = View       (BaseNum r)
type LayoutAtom' r = LayoutAtom (BaseNum r)
type LayoutDim'  r = LayoutDim  (BaseNum r)

layout :: Rules r => Width' r -> Cursor Text Word -> r (View' r)
layout maxWidth c = do
  layoutFull maxWidth <$> computeFull <$> inlineAtoms (highlightSelection c)

highlightSelection :: Cursor Text Word -> Layout
highlightSelection = justTheTip . highlightPath True
  where
    justTheTip :: Tree Text ((Word, Selection), Bool) -> Layout
    justTheTip = mapAll $ \ ((id, sel), onPath) -> LayoutInfo
      { Redoak.Layout.ident = id
      , Redoak.Layout.selection = case (onPath, sel) of
          (True, Select r) -> Just r
          _                -> Nothing
      }

    highlightPath :: Bool
             -> Cursor Text Word
             -> Tree Text ((Word, Selection), Bool)
    highlightPath onPath (a@(_, sel) :< e) = ((a, onPath) :<) $ case e of
      Atom a  -> Atom a
      Node ts -> Node $ case (onPath, sel) of
        (True, Descend i) -> fmap (uncurry highlightPath)
                               $ fmap (first (== i))
                               $ snd
                               $ mapAccumL (\ count elem -> (count + 1, (count, elem))) 0 ts
        _                 -> highlightPath False <$> ts

inlineAtoms :: Rules r => Layout -> r (LayoutAtom' r)
inlineAtoms = from unCofree8Bifunctor `mapMOf` bimapM f return
  where f s = (s,) <$> inlineText s

computeFull :: Adequate n => LayoutAtom n -> LayoutDim n
computeFull (info :< e) = (info, dim) :< e' where
  (dim, e') = case e of
    Atom (a, dim) -> (dim, Atom a)
    Node ts -> (dim, Node fulls)
      where
        fulls = computeFull <$> ts
        fullDims = fmap (snd . extract . Cofree8Comonad) fulls
        maxWidth  = maximum $ W 0 <| fmap fst fullDims
        maxHeight = maximum $ H 0 <| fmap snd fullDims
        dim = if maxHeight <= maxInlineHeight
                then (sum $ fmap ((+ inlinePad) . fst) fullDims, maxHeight)
                else (maxWidth, sum $ fmap snd fullDims)

-- all nodes must have full size computed
layoutFull :: Adequate n => Width n -> LayoutDim n -> View n
layoutFull mw t@((info, (w, h)) :< e) =
  if w <= mw
  then defaultView Horizontal
  else case e of
    Atom a -> defaultView Vertical
    Node ts -> case viewl ts of
      EmptyL -> defaultView Vertical
      first S.:< rest ->
        let views = layoutFull mw first
              <| fmap (layoutFull (mw - indentWidth)) rest in
        let fullDim = ( maximum $ W 0 <| fmap (fst . dim . extract . Cofree8Comonad) views
                      , sum $ fmap (snd . dim . extract . Cofree8Comonad) views
                      ) in
        sel $ makeViewInfo Vertical (info, fullDim) :< Node views

  where sel = select $ selection info
        defaultView dir = layoutHomogenous dir t

layoutHomogenous :: Adequate n => Direction -> LayoutDim n -> View n
layoutHomogenous dir (ann@(info, _) :< e) =
  select (selection info)
    $ makeViewInfo dir ann :< second (layoutHomogenous dir) e

select :: Adequate n => Maybe (Range Word) -> View n -> View n
select Nothing t = t
select (Just (start, end)) (info :< e) = uncurry (:<) $ case e of
  Atom a -> (info',) $ Node $
    [ lInfo :< Atom lPart
    , selInfo :< Atom selPart
    , rInfo :< Atom rPart
    ]
    where (lPart, selPart, rPart) = split a
          lInfo = ViewInfo
            { Redoak.View.ident = Nothing
            , classes = []
            , dim = (W 0, H 0)
            , pos = (X 0, Y 0)
            }
          rInfo = lInfo
          info' = info { classes = ["atom-selection"] }

  Node ts ->
    (info, Node $ mconcat [lPart, [selInfo :< Node selPart], rPart])
    where (lPart, selPart, rPart) = split ts

  where front = min start end
        back  = max start end
        selInfo = ViewInfo
          { Redoak.View.ident = Just "selection"
          , classes = [if start <= end then "right" else "left"]
          , dim = (W 0, H 0)
          , pos = (X 0, Y 0)
          }
        split :: IsSequence s => s -> (s, s, s)
        split s =
          ( SS.take (fromIntegral front) s
          , SS.take (fromIntegral $ back - front) $
            SS.drop (fromIntegral front) s
          , SS.drop (fromIntegral back) s
          )

dirClass :: Direction -> Text
dirClass Horizontal = "horizontal"
dirClass Vertical   = "vertical"

makeViewInfo :: Adequate n
             => Direction -> (LayoutInfo, Dimensions n) -> ViewInfo n
makeViewInfo dir (LayoutInfo id sel, dim) = ViewInfo
  { Redoak.View.ident = Just $ T.pack $ show id
  , classes = ["content", dirClass dir]
  , dim = dim
  , pos = (X 0, Y 0)
  }
