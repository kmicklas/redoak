{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Reflex.Parsec where

import Control.Monad
import Control.Monad.Fix
import Data.Bifunctor
import Text.Parsec
import Reflex


--
-- Coroutines
--

--- Eliminator on streams that can return early; one sort of coroutine
newtype AntiListT t m r =
  AntiListT { resume :: m (Either (t -> AntiListT t m r) r) }

instance Functor f => Functor (AntiListT t f) where
  fmap f = go where
    go (AntiListT g) = AntiListT $ fmap (bimap (go .) f) g

instance Monad a => Applicative (AntiListT t a) where
  pure = AntiListT . pure . Right
  (<*>) = ap

instance Monad m => Monad (AntiListT t m) where
  return = pure
  (AntiListT a) >>= f = AntiListT $ a >>= \case
    Left  a' -> resume $ await $ a' >=> f
    Right a' -> resume $ f a'

await :: Monad m => (t -> AntiListT t m r) -> AntiListT t m r
await = AntiListT . pure . Left

neverCr :: Monad m => AntiListT a m r
neverCr = AntiListT $ pure $ Left $ const neverCr


--
-- Never-Ending Coroutines
--

newtype AntiListsT t m r = AntiListsT (AntiListT t m (AntiListsT t m r, r))

repeatForever :: Functor m => AntiListT a m r -> AntiListsT a m r
repeatForever orgCr = looped where
  looped = AntiListsT $ fmap (looped,) orgCr

neverCrs :: Monad m => AntiListsT a m r
neverCrs = repeatForever neverCr

oneShot :: Monad m => AntiListT a m r -> AntiListsT a m r
oneShot = AntiListsT . fmap (neverCrs,)

foldCoroutineForever :: forall t a m r
              .  Reflex t
              => (MonadHold t m, MonadFix m)
              => AntiListT a (PushM t) r
              -> Event t a
              -> m (Event t r)
foldCoroutineForever = foldCoroutines . repeatForever

foldCoroutineOnce :: forall t a m r
              .  Reflex t
              => (MonadHold t m, MonadFix m)
              => AntiListT a (PushM t) r
              -> Event t a
              -> m (Event t r)
foldCoroutineOnce = foldCoroutines . oneShot

foldCoroutines :: forall t a m r
               .  Reflex t
               => (MonadHold t m, MonadFix m)
               => AntiListsT a (PushM t) r
               -> Event t a
               -> m (Event t r)
foldCoroutines orgCr = mealyDynMaybeM feeder orgCr
  where 
    feeder :: a
           -> AntiListsT a (PushM t) r
           -> PushM t (AntiListsT a (PushM t) r, Maybe r)
    feeder tok (AntiListsT (AntiListT cr)) = flip fmap cr $ \case
      Left  cr'        -> (AntiListsT $ cr' tok , Nothing)
      Right (crs, ret) -> (crs                  , Just ret)
                          

mealyDynMaybeM :: (Reflex t, MonadHold t m, MonadFix m)
               => (a -> b -> PushM t (b, Maybe r))
               -> b
               -> Event t a
               -> m (Event t r)
mealyDynMaybeM f z e = push return <$> fmap snd
  <$> updated <$> foldDynM (\a (b, _) -> f a b) (z, Nothing) e


--
-- Parsec
--

data VirtualStream a = VirtualStream

type VirtualParsectT t u m r = ParsecT (VirtualStream t) u (AntiListT t m) r

instance Monad m => Stream (VirtualStream t) (AntiListT t m) t where
  uncons _ = await $ \t -> return $ Just (t, VirtualStream)

resumeParsec :: Monad m
             => VirtualParsectT t u m r
             -> u
             -> SourceName
             -> AntiListT t m (Either ParseError r)
resumeParsec p u s = runParserT p u s VirtualStream
