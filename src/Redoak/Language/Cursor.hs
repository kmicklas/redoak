{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Redoak.Language.Cursor where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Constraint
import Data.Functor.Identity

import Control.Comonad.Cofree8
import Data.Functor8

import Redoak.Language.Base


type Range n = (n, n)

data Tip n
  = Single n
  | Range (Range n)
  deriving (Eq, Ord, Show, Functor)

data SelectionInner n
  = Descend n
  | Select (Tip n)
  deriving (Eq, Ord, Show, Functor)

type Selection = SelectionInner Word

type Path = ([Word], Tip Word)

type Cursor f0 f1 f2 f3 f4 f5 f6 f7  n ann =
  Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n (ann, Selection)

type EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  StateT (Cursor f0 f1 f2 f3 f4 f5 f6 f7  n ann) m r
type Edit f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  EditT Identity f0 f1 f2 f3 f4 f5 f6 f7  n ann r

type MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  EditT (MaybeT m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
type MaybeEdit  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  MaybeEditT Identity  f0 f1 f2 f3 f4 f5 f6 f7  n ann r

ntfCls :: forall a. NonTerminal a :- Functor8 a
ntfCls = cls

index :: forall f a . NonTerminal f => f a a a a a a a a -> Word -> a
index nt i = runIdentity $ indexC nt i
  Identity Identity Identity Identity Identity Identity Identity Identity

indexStateC :: forall m ann r f  f0 f1 f2 f3 f4 f5 f6 f7
            .  ( NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
               , Functor m, NonTerminal f)
            => Word
            -> (forall n
                .  EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r)
            -> StateT (Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 (ann, Selection)) m r
indexStateC i f = StateT $ \nt -> unPairT $ modifyC nt i go go go go go go go go
  where
    go :: forall n'
       .  Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann
       -> PairT r m (Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann)
    go x = PairT $ runStateT f x

assertCanRecur :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, NonTerminal f, Monad m)
               => StateT (Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 (ann, Selection)) m ()
assertCanRecur = do
  nt <- get
  --- based on Control.Exception.Base.evaluate
  unless (canDescend nt) $ return =<< (return $! error "path is too deep")


path :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
     .  NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
     => Cursor  f0 f1 f2 f3 f4 f5 f6 f7 n ann -> Path
path l = foldPoly ntfCls go l where
  go :: forall f . NonTerminal f
     => Cofree8Inner' f f0 f1 f2 f3 f4 f5 f6 f7 (ann, Selection) -> Path
  go x = case (getAnn l, x) of
    ((_, Descend i), nt) -> first (i :) $ index (foldFPoly path nt) i
    ((_, Select r),  _)  -> ([], r)

local :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
      => (forall n'. EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n' ann r)
      -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
local f = do
  (_, sel) <- getAnn <$> get
  case sel of
    (Select _) -> f
    (Descend i) -> mapStatePoly ntfCls $ do
      assertCanRecur
      indexStateC i $ local f

-- | Select the node which we're currently inside
ascend :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
       .  (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
       => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
ascend = (getAnn <$> get) >>= \case
    (_, Select _) -> mzero
    (_, Descend i) -> go0 i
  where
    go0 :: forall n
        . (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
        => Word
        -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
    go0 i = do
      (foundIt, useRange) <- mapStatePoly ntfCls $ do
        assertCanRecur
        nextDepth <- indexStateC i $ (getAnn <$> get) >>= \case
          (a, Select _)  -> return True
          (a, Descend i') -> go0 i' >> return False
        nt <- get
        return (nextDepth, canSelectRange nt)
      when foundIt $ modify $ modifyAnn $ second $ \(Descend _) ->
        Select $ if useRange
                 then Range (i, i + 1)
                 else Single $ i + 1

-- | Descend into selection, if only one element is selected
descend :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
        => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
descend = local $ do
  i <- guardSingle
  guard =<< foldPoly ntfCls canDescend <$> get
  modify $ modifyAnn $ second $ \(Select _) -> Descend i

-- | Go back to editing parent, right of current position
pop :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
    => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
pop = ascend >> selectNoneEnd

localMove :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
          => (Int -> Tip Int -> Maybe (Tip Int))
          -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
localMove f = local $ adjustSelection (\x y -> MaybeT $ return $ f x y) mzero

adjustSelection :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
                => (Int -> Tip Int -> m (Tip Int))
                -> m ()
                -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
adjustSelection f bad = catchInvalidNode
  (do
      (_, Select tip) <- getAnn <$> get
      len <- fromIntegral <$> foldPoly ntfCls Redoak.Language.Base.length <$> get
      tip' <- lift $ f len $ fromIntegral <$> tip
      modify $ modifyAnn $ fmap $ \(Select _) -> Select $ fromIntegral <$> tip')
  return
  (lift bad)

catchInvalidNode :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
                 => EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann a
                 -> (a -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ())
                 -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
                 -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
catchInvalidNode edit good bad = do
  (a, s') <- lift . runStateT edit =<< get
  let (_, sel) = getAnn s'
      len = foldPoly ntfCls Redoak.Language.Base.length s'
  let valid = case sel of
        Descend p                   -> 0 <= p && p < fromIntegral len
        Select (Single p)           -> 0 <= p && p < fromIntegral len
        Select (Range (start, end)) -> min start end >= 0 && max start end <= fromIntegral len
  if valid
    then do
       put s'
       good a
    else bad

localMoveR :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
           => (Int -> Range Int -> Range Int)
           -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
localMoveR f = localMove $ \x -> \case
  Single _ -> Nothing
  Range  y -> Just $ Range $ f x y

switchBounds :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
             => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
switchBounds = localMoveR $ \_ (start, end) -> (end, start)

startMin :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
         => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
startMin = localMoveR $ \ _ (_, end) -> (0, end)

endMax :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
       => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
endMax = localMoveR $ \ size (start, _) -> (start, size)

selectAll :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
          => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
selectAll = localMoveR $ \ size (_, end) -> (0, size)

selectNoneStart :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
                => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
selectNoneStart = localMoveR $ \ _ (start, _) -> (start, start)

selectNoneEnd :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
              => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
selectNoneEnd = localMoveR $ \ _ (_, end) -> (end, end)

shiftLeft :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
          => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
shiftLeft = localMove $ const $ Just . \case
  Single pos         -> Single $ pos - 1
  Range (start, end) -> Range (start - 1, end - 1)

shiftRight :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
           => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
shiftRight = localMove $ const $ Just . \case
  Single pos         -> Single $ pos + 1
  Range (start, end) -> Range (start + 1, end + 1)

moveLeft :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
         => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
moveLeft = localMoveR $ \ _ (start, end) -> (start, end - 1)

moveRight :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
          => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
moveRight = localMoveR $ \ _ (start, end) -> (start, end + 1)

unCursor :: NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
         => Cursor  f0 f1 f2 f3 f4 f5 f6 f7  n ann
         -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ann
unCursor = mapAll fst

initCursor :: forall f0 f1 f2 f3 f4 f5 f6 f7  n ann
           .  NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
           => Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ann
           -> Cursor  f0 f1 f2 f3 f4 f5 f6 f7  n ann
initCursor = go . mapAll (\ann -> (ann, undefined)) where
  go :: forall f0 f1 f2 f3 f4 f5 f6 f7  n ann
     .  NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
     => Cursor  f0 f1 f2 f3 f4 f5 f6 f7  n ann
     -> Cursor  f0 f1 f2 f3 f4 f5 f6 f7  n ann
  go e = modifyAnn (second $ const sel') e'
    where
      (useRange, e') = mapPolyF ntfCls go1 e
      go1 :: forall f . NonTerminal f
          => Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7  (ann, Selection)
          -> (Bool, Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7  (ann, Selection))
      go1 nt = (canSelectRange nt, mapFPoly go nt)
      sel' = Select $ if useRange
                      then Range (0, 0)
                      else Single 0

isEmpty :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
          => EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann Bool
isEmpty = local $ do
  (_, Select sel) <- getAnn <$> get
  return $ case sel of
    Single _           -> False
    Range (start, end) -> start == end

guardSingle :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
            => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann Word
guardSingle = do
  (_, Select sel) <- getAnn <$> get
  case sel of
    Single pos         -> return pos
    Range (start, end) -> do guard $ diff start end == 1
                             return $ min start end

data SelectionPreference = SelectNone
                         | SelectOne
  deriving Eq

data LeafTraverseInternalError = SelectMultipleFatal
                               | EndofSelection SelectionPreference
  deriving Eq

data Direction = Leftwards | Rightwards

-- | Leaf traversal helper
leafMove :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
         .  (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
         => Direction
         -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
leafMove direction = mapStateT exceptToMaybeT go0 where
  incOrDec :: Num a => a -> a
  incOrDec = case direction of
        Leftwards  -> (flip (-) 1)
        Rightwards -> (+ 1)
  go0 :: forall n
      . (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
      => EditT (ExceptT LeafTraverseInternalError m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
  go0 = (snd <$> getAnn <$> get) >>= \case
    (Select t) -> do
      pref <- case t of
        Range (start, end) -> case diff start end of
          0 -> return SelectNone
          1 -> return SelectOne
          _ -> lift $ throwE SelectMultipleFatal
        Single _           -> return SelectOne
      adjustSelection (const $ lift . return . fmap incOrDec) $ throwE $ EndofSelection pref

    (Descend i) -> do
      goSideways <- mapStatePoly ntfCls $ do
        assertCanRecur
        nt <- get
        let go :: forall n'
               .  Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann
               -> ExceptT LeafTraverseInternalError m (Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann)
            go x = (\((),s) -> s) <$> runStateT go0 x
            recur = modifyC nt i go go go go go go go go
        (nt', pref) <- lift $ flip mapExceptT recur $ fmap $ \case
          Left SelectMultipleFatal   -> Left SelectMultipleFatal
          Left (EndofSelection pref) -> Right (nt, Just pref)
          Right nt'                  -> Right (nt', Nothing)
        put nt'
        return pref
      useRange <- mapStatePoly ntfCls $ canSelectRange <$> get
      case goSideways of
        Nothing   -> return ()
        Just pref -> catchInvalidNode

          (modify $ modifyAnn $ second $ case (pref, useRange) of
            (SelectNone, True)  -> const $ Select $ case direction of
              Leftwards  -> Range (i, i)
              Rightwards -> Range (i + 1, i + 1)
            (SelectOne,  True)  -> const $ Select $ fmap incOrDec $ Range (i, i + 1)
            (SelectOne,  False) -> const $ Select $ fmap incOrDec $ Single i
            (SelectNone, False) -> const $ Select $ fmap incOrDec $ Single i)

          (\() -> unless ((SelectNone, True) == (pref, useRange)) $ case direction of
              Leftwards  -> descendAllInternal pref Rightwards
              Rightwards -> descendAllInternal pref Leftwards)

          (lift $ throwE $ EndofSelection pref)


descendAll :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
           .  (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
           => SelectionPreference
           -> Direction
           -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
descendAll pref direction = do
  i <- guardSingle
  guard =<< foldPoly ntfCls canDescend <$> get
  modify $ modifyAnn $ second $ const $ Descend i
  descendAllInternal pref direction

-- | Go to edge leaf
descendAllInternal :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
                   => SelectionPreference
                   -> Direction
                   -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
descendAllInternal pref direction = do
  (_, sel) <- getAnn <$> get
  len <- foldPoly ntfCls Redoak.Language.Base.length <$> get
  continue <- case sel of
    (Select tip) -> do
      canDescend' <- foldPoly ntfCls canDescend <$> get
      return $ case (canDescend', len > 0) of
        (True, True) -> Right $ case direction of
          Leftwards  -> 0       -- leftmost
          Rightwards -> len - 1 -- rightmost
        (_, lenNonZero) -> Left $ case (tip, pref, lenNonZero, direction) of
          (Single _, _,          False, _)          -> Single 0
          (Single _, _,          True,  Leftwards)  -> Single 0
          (Single _, _,          True,  Rightwards) -> Single $ len - 1
          (Range  _, _,          False, _)          -> Range (0, 0)
          (Range  _, SelectNone, True,  Leftwards)  -> Range (0, 0)
          (Range  _, SelectNone, True,  Rightwards) -> Range (len, len)
          (Range  _, SelectOne,  True,  Leftwards)  -> Range (0, 1)
          (Range  _, SelectOne,  True,  Rightwards) -> Range (len - 1, len)
    (Descend i) -> return $ Right i
  case continue of
    Left tip' -> modify $ modifyAnn $ second $ const $ Select tip'
    Right i -> do
      modify $ modifyAnn $ second $ const $ Descend i
      mapStatePoly ntfCls $ do
        assertCanRecur
        indexStateC i $ descendAllInternal pref direction
