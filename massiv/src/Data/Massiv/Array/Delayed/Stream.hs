{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Stream
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Stream
  ( DS(..)
  , Array (..)
  , toStreamArray
  , toSteps
  , fromSteps
  , fromStepsM
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Coerce
import Data.Massiv.Array.Delayed.Pull
import qualified Data.Massiv.Vector.Stream as S
import Data.Massiv.Core.Common
import GHC.Exts
import Prelude hiding (take, drop)
import Data.Vector.Fusion.Bundle.Size (upperBound)

-- | Delayed stream array that represents a sequence of values that can be loaded
-- sequentially. Important distinction from other arrays is that its size might no be
-- known until it is computed.
data DS = DS

newtype instance Array DS Ix1 e = DSArray
  { dsArray :: S.Steps S.Id e
  }

-- | /O(1)/ - Convert delayed stream array into `Steps`.
--
-- @since 0.4.1
toSteps :: Array DS Ix1 e -> Steps Id e
toSteps = coerce
{-# INLINE toSteps #-}

-- | /O(1)/ - Convert `Steps` into delayed stream array
--
-- @since 0.4.1
fromSteps :: Steps Id e -> Array DS Ix1 e
fromSteps = coerce
{-# INLINE fromSteps #-}

-- | /O(1)/ - Convert monadic `Steps` into delayed stream array
--
-- @since 0.5.0
fromStepsM :: Monad m => Steps m e -> m (Array DS Ix1 e)
fromStepsM = fmap DSArray . S.transSteps
{-# INLINE fromStepsM #-}


instance Functor (Array DS Ix1) where

  fmap f = coerce . fmap f . dsArray
  {-# INLINE fmap #-}

instance Applicative (Array DS Ix1) where

  pure = fromSteps . S.singleton
  {-# INLINE pure #-}

  (<*>) a1 a2 = fromSteps (S.zipWith ($) (coerce a1) (coerce a2))
  {-# INLINE (<*>) #-}

#if MIN_VERSION_base(4,10,0)
  liftA2 f a1 a2 = fromSteps (S.zipWith f (coerce a1) (coerce a2))
  {-# INLINE liftA2 #-}
#endif

instance Monad (Array DS Ix1) where

  return = fromSteps . S.singleton
  {-# INLINE return #-}

  (>>=) arr f = coerce (S.concatMap (coerce . f) (dsArray arr))
  {-# INLINE (>>=) #-}


instance Foldable (Array DS Ix1) where

  foldr f acc = S.foldr f acc . toSteps
  {-# INLINE foldr #-}

  length = S.length . coerce
  {-# INLINE length #-}

  -- TODO: add more


instance Semigroup (Array DS Ix1 e) where

  (<>) a1 a2 = fromSteps (coerce a1 `S.append` coerce a2)
  {-# INLINE (<>) #-}


instance Monoid (Array DS Ix1 e) where

  mempty = DSArray S.empty
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance IsList (Array DS Ix1 e) where
  type Item (Array DS Ix1 e) = e

  fromList = fromSteps . S.fromList
  {-# INLINE fromList #-}

  fromListN n = fromSteps . S.fromListN n
  {-# INLINE fromListN #-}

  toList = S.toList . coerce
  {-# INLINE toList #-}


instance S.Stream DS Ix1 e where
  toStream = coerce
  {-# INLINE toStream #-}


-- | Flatten an array into a stream of values.
--
-- @since 0.4.1
toStreamArray :: Source r ix e => Array r ix e -> Array DS Ix1 e
toStreamArray = DSArray . S.steps
{-# INLINE toStreamArray #-}

instance Construct DS Ix1 e where
  setComp _ arr = arr
  {-# INLINE setComp #-}

  makeArrayLinear _ (Sz k) = fromSteps . S.generate k
  {-# INLINE makeArrayLinear #-}


instance Extract DS Ix1 e where
  unsafeExtract sIx newSz = fromSteps . S.slice sIx (unSz newSz) . dsArray
  {-# INLINE unsafeExtract #-}

-- | /O(n)/ - `size` implementation.
instance Load DS Ix1 e where
  size = coerce . S.length . coerce
  {-# INLINE size #-}

  maxSize = coerce . upperBound . stepsSize . dsArray
  {-# INLINE maxSize #-}

  getComp _ = Seq
  {-# INLINE getComp #-}

  loadArrayM _scheduler arr uWrite =
    case stepsSize (dsArray arr) of
      S.Exact _ ->
        void $ S.foldlM (\i e -> uWrite i e >> pure (i + 1)) 0 (S.transStepsId (coerce arr))
      _ -> error "Loading Stream array is not supported with loadArrayM"
  {-# INLINE loadArrayM #-}

  unsafeLoadIntoS marr (DSArray sts) =
    S.unstreamIntoM marr (stepsSize sts) (stepsStream sts)
  {-# INLINE unsafeLoadIntoS #-}

  unsafeLoadInto marr arr = liftIO $ unsafeLoadIntoS marr arr
  {-# INLINE unsafeLoadInto #-}


-- cons :: e -> Array DS Ix1 e -> Array DS Ix1 e
-- cons e = coerce . S.cons e . dsArray
-- {-# INLINE cons #-}

-- uncons :: Array DS Ix1 e -> Maybe (e, Array DS Ix1 e)
-- uncons = coerce . S.uncons . dsArray
-- {-# INLINE uncons #-}

-- snoc :: Array DS Ix1 e -> e -> Array DS Ix1 e
-- snoc (DSArray sts) e = DSArray (S.snoc sts e)
-- {-# INLINE snoc #-}


-- TODO: skip the stride while loading
-- instance StrideLoad DS Ix1 e where
--   loadArrayWithStrideM scheduler stride resultSize arr uWrite =
--     let strideIx = unStride stride
--         DIArray (DArray _ _ f) = arr
--     in loopM_ 0 (< numWorkers scheduler) (+ 1) $ \ !start ->
--           scheduleWork scheduler $
--           iterLinearM_ resultSize start (totalElem resultSize) (numWorkers scheduler) (<) $
--             \ !i ix -> uWrite i (f (liftIndex2 (*) strideIx ix))
--   {-# INLINE loadArrayWithStrideM #-}


