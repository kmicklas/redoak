{-# LANGUAGE BangPatterns #-}
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
import GHC.TypeLits

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
type CursorInner f f0 f1 f2 f3 f4 f5 f6 f7  ann =
  Cofree8Inner' f f0 f1 f2 f3 f4 f5 f6 f7  (ann, Selection)

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

modifyStateC :: forall m ann r f  f0 f1 f2 f3 f4 f5 f6 f7
             .  ( NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
                , Functor m, NonTerminal f)
             => Word
             -> (forall n. KnownNat n
                 =>  EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r)
             -> StateT (CursorInner f  f0 f1 f2 f3 f4 f5 f6 f7  ann) m r
modifyStateC i f = StateT $ \nt -> unPairT $ modifyC nt i go go go go go go go go
  where
    go :: forall n'. KnownNat n'
       => Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann
       -> PairT r m (Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann)
    go x = PairT $ runStateT f x

assertCanRecur :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, NonTerminal f, Monad m)
               => StateT (CursorInner f  f0 f1 f2 f3 f4 f5 f6 f7  ann) m ()
assertCanRecur = do
  nt <- get
  unless (canDescend nt) $ do
   !_ <- error "path is too deep"
   return ()

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
      modifyStateC i $ local f

-- | Like `local`, but descend *into* the final selection
local' :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
      => (forall n'. MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n' ann r)
      -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
local' f = do
  (_, sel) <- getAnn <$> get
  case sel of
    (Select _) -> do
      i <- guardSingle
      mapStatePoly ntfCls $ do
        guard =<< canDescend <$> get --- not assert
        modifyStateC i f
    (Descend i) -> mapStatePoly ntfCls $ do
      assertCanRecur
      modifyStateC i $ local f


-- | Select the node which we're currently inside
ascend :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
       .  (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
       => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
ascend = (snd <$> getAnn <$> get) >>= \case
    (Select _) -> mzero
    (Descend i) -> go i
  where
    go :: forall n
       . (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
       => Word
       -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
    go i = do
      (foundIt, useRange) <- mapStatePoly ntfCls $ do
        assertCanRecur
        nextDepth <- modifyStateC i $ (getAnn <$> get) >>= \case
          (a, Select _)  -> return True
          (a, Descend i') -> go i' >> return False
        nt <- get
        return (nextDepth, canSelectRange nt)
      when foundIt $ modify $ modifyAnn $ second $ \(Descend _) ->
        Select $ if useRange
                 then Range (i, i + 1)
                 else Single i

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
localMove f = local $ do
  sel' <- adjustSelection (\x y -> MaybeT $ return $ f x y)
  guard =<< checkSetSel (Select sel')

adjustSelection :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
                => (Int -> Tip Int -> m (Tip Int))
                -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann (Tip Int)
adjustSelection f = do
  (_, Select tip) <- getAnn <$> get
  len <- fromIntegral <$> foldPoly ntfCls Redoak.Language.Base.length <$> get
  lift $ f len $ fromIntegral <$> tip

checkSetSel :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
            => SelectionInner Int
            -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann Bool
checkSetSel sel = do
  len         <- foldPoly ntfCls Redoak.Language.Base.length <$> get
  canDescend' <- foldPoly ntfCls canDescend                  <$> get
  let valid = case sel of
        Descend p                   -> 0 <= p && p < fromIntegral len && canDescend'
        Select (Single p)           -> 0 <= p && p < fromIntegral len
        Select (Range (start, end)) -> min start end >= 0 && max start end <= fromIntegral len
  when valid $ modify $ modifyAnn $ fmap $ \_ -> fromIntegral <$> sel
  return valid

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

selectOne :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
          => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
selectOne = selectNoneEnd >> maybeEdit moveRight (moveLeft >> switchBounds)

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

data EmptyTraverseInternalError = CantSelectNone
  deriving Eq

data Direction = Leftwards | Rightwards

catchStateExceptT :: Monad m
                  => StateT s (ExceptT e m) a
                  -> StateT s m (Either e a)
catchStateExceptT m = do
  s <- get
  lift (runExceptT $ runStateT m s) >>= \case
    Left  e      -> return $ Left e
    Right (a, s) -> put s >> return (Right a)

catchStateExceptT' :: Monad m
                   => StateT s (ExceptT e m) a
                   -> StateT s (ExceptT e' m) (Either e a)
catchStateExceptT' = mapStateT lift . catchStateExceptT


wand :: Monad m => Bool -> m Bool -> m Bool
wand cond action = if cond then action else return False

-- | Move the Cursor between Leaves like a text cursor
emptyMove :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
         .  (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
         => Direction
         -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
emptyMove direction = mapStateT exceptToMaybeT go where
  leftDec :: Num a => a -> a
  leftDec = case direction of
        Leftwards  -> (flip (-) 1)
        Rightwards -> id

  guardSelectRange = do
    canSelectNone <- foldPoly ntfCls canSelectRange <$> get
    unless canSelectNone $ lift $ throwE CantSelectNone

  go :: forall n
     . (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
     => EditT (ExceptT EmptyTraverseInternalError m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
  go = (snd <$> getAnn <$> get) >>= \case
    (Select t) -> do
      start <- case t of
        Range (start, end) -> return start
        Single i           -> lift $ throwE CantSelectNone

      let i = case direction of
            Leftwards  -> start - 1
            Rightwards -> start -- asymmetrical on purpose

      canSelectWholeAdjecent <- checkSetSel $ fromIntegral <$> Select (Range (i, i + 1))

      if not canSelectWholeAdjecent
        then lift $ throwE CantSelectNone
        else do
          modify $ modifyAnn $ second $ \(Select _) -> Descend i
          canDescend' <- foldPoly ntfCls canDescend <$> get
          sucess <- wand canDescend' $ mapStatePoly ntfCls $ do
            res <- catchStateExceptT' $ modifyStateC i $ do
              guardSelectRange
              len <- foldPoly ntfCls Redoak.Language.Base.length <$> get
              modify $ modifyAnn $ second $ const $ Select $ case direction of
                -- flipped
                Leftwards  -> Range (len, len)
                Rightwards -> Range (0, 0)
            return $ case res of
              Left  _ -> False
              Right _ -> True
          unless sucess $ modify $ modifyAnn $ second $ const $ Select $ let
            i' = case direction of
              Leftwards  -> start - 1
              Rightwards -> start + 1
            in Range (i', i')

    (Descend i) -> do
      goSideways <- mapStatePoly ntfCls $ do
        assertCanRecur
        catchStateExceptT' $ modifyStateC i go
      case goSideways of
        Right _ -> return ()
        Left  _ -> do
          guardSelectRange
          modify $ modifyAnn $ second $ const $ Select $ case direction of
            Leftwards  -> Range (i, i)
            Rightwards -> Range (i + 1, i + 1)

data LeafTraverseInternalError = EndOfSelection
  deriving Eq

-- | Move a singleton selection between leaves
leafMove :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
         .  (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
         => Direction
         -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
leafMove direction = mapStateT exceptToMaybeT go where
  incOrDec :: Num a => a -> a
  incOrDec = case direction of
        Leftwards  -> (flip (-) 1)
        Rightwards -> (+1)

  trySides :: forall n
           .  (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
           => Word
           -> EditT (ExceptT LeafTraverseInternalError m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
  trySides i' = do
    useRange <- foldPoly ntfCls canSelectRange <$> get
    let i = incOrDec i'
    canSelectWholeAdjecent <- checkSetSel $ fromIntegral <$> if useRange
      then Select $ Range (i, i + 1)
      else Select $ Single i
    if not canSelectWholeAdjecent
      then lift $ throwE $ EndOfSelection
      else descendAll $ case direction of
             Leftwards  -> Rightwards
             Rightwards -> Leftwards

  go :: forall n
     . (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
     => EditT (ExceptT LeafTraverseInternalError m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
  go = (snd <$> getAnn <$> get) >>= \case
    (Select t) -> do
      let start = case t of
            Range (start, end) -> min start end
            Single i           -> i
      trySides start

    (Descend i) -> do
      goSideways <- mapStatePoly ntfCls $ do
        assertCanRecur
        catchStateExceptT' $ modifyStateC i go
      case goSideways of
        Right _ -> return ()
        Left  _ -> trySides i -- pretend had selection on this level

-- | Go to edge leaf
descendAll :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
           => Direction
           -> EditT (ExceptT LeafTraverseInternalError m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
descendAll direction = do
  (_, sel) <- getAnn <$> get
  len <- foldPoly ntfCls Redoak.Language.Base.length <$> get
  continue <- case sel of
    (Select tip) -> do
      canDescend' <- foldPoly ntfCls canDescend <$> get
      when (len == 0) $ lift $ throwE EndOfSelection
      return $ case canDescend' of
        True  -> Right $ case direction of
          Leftwards     -> 0       -- leftmost
          Rightwards    -> len - 1 -- rightmost
        False -> Left $ case (tip, direction) of
          (Single _, Leftwards)  -> Single 0
          (Single _, Rightwards) -> Single $ len - 1
          (Range  _, Leftwards)  -> Range (0, 1)
          (Range  _, Rightwards) -> Range (len - 1, len)
    (Descend i) -> return $ Right i
  case continue of
    Left tip' -> modify $ modifyAnn $ second $ const $ Select tip'
    Right i -> do
      modify $ modifyAnn $ second $ const $ Descend i
      wentFurther <- mapStatePoly ntfCls $ do
        assertCanRecur
        catchStateExceptT' $ modifyStateC i $ descendAll direction
      case wentFurther of
        Right _ -> return ()
        Left  _ -> do
          useRange <- foldPoly ntfCls canSelectRange <$> get
          -- In case was `descend i` Already
          modify $ modifyAnn $ second $ const $ Select $ if useRange
                                                         then Range (i, i)
                                                         else Single i

delete :: (NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m)
       => EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
delete = local $ do
  (_, Select tip) <- getAnn <$> get
  let tip' = case tip of
        Single i     -> (i, i + 1)
        Range (a, b) -> (min a b, max a b)
  mapStatePoly ntfCls $ modify $ deleteX tip'
