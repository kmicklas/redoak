{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Redoak.Language
  ( module Redoak.Language.Base
  , module Redoak.Language.Cursor
  , module Redoak.Language.Fresh
  , Term(..)
  , RootTerm(..)
  , Language(..)
  , Renderable(..)
  , RenderableNonTerminal(..)
  ) where

import Data.Text (Text)
import Reflex
import Reflex.Dom

import Redoak.Event
import Redoak.Language.Base
import Redoak.Language.Cursor
import Redoak.Language.Fresh

type Term f0 f1 f2 f3 f4 f5 f6 f7 n =
  Cursor
    f0 f1 f2 f3 f4 f5 f6 f7
    n
    (Ann f0 f1 f2 f3 f4 f5 f6 f7)

type RootTerm f0 f1 f2 f3 f4 f5 f6 f7 = Term f0 f1 f2 f3 f4 f5 f6 f7 7

class NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
      => Language f0 f1 f2 f3 f4 f5 f6 f7 where

  type Ann f0 f1 f2 f3 f4 f5 f6 f7 :: *

  type Accum f0 f1 f2 f3 f4 f5 f6 f7 :: *

  handleEvent :: KeyEvent
              -> ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
                 , Accum     f0 f1 f2 f3 f4 f5 f6 f7)
              -> ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
                 , Accum     f0 f1 f2 f3 f4 f5 f6 f7)


  getMessage :: MonadWidget t m
             => ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
                , Accum     f0 f1 f2 f3 f4 f5 f6 f7)
             -> ( Text  -- mode/status/whatever
                , m ()) -- some widget to display

-- | Also squashes
class (NonTerminal f, NonTerminal g) => RenderableNonTerminal f g where
  convertNT :: forall a b0 b1 b2 b3 b4 b5 b6 b7
            .  f a a a a a a a a
            -> g b0 b1 b2 b3 b4 b5 b6 a

class ( Language f0 f1 f2 f3 f4 f5 f6 f7
      , Language g0 g1 g2 g3 g4 g5 g6 g7)
      => Renderable  f0 f1 f2 f3 f4 f5 f6 f7  g0 g1 g2 g3 g4 g5 g6 g7 where

  convert :: ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
             , Accum     f0 f1 f2 f3 f4 f5 f6 f7)
          -> ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
             , Accum     f0 f1 f2 f3 f4 f5 f6 f7)

instance Language f0 f1 f2 f3 f4 f5 f6 f7
         => Renderable  f0 f1 f2 f3 f4 f5 f6 f7  f0 f1 f2 f3 f4 f5 f6 f7 where

  convert = id
