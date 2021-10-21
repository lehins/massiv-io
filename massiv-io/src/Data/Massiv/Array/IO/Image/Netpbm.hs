{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image.Netpbm
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.Netpbm
  ( -- * Netpbm formats
    -- ** PBM
    PBM(..)
    -- ** PGM
  , PGM(..)
    -- ** PPM
  , PPM(..)
  , decodeNetpbmImage
  , decodeNetpbmImageSequence
  , decodeAutoNetpbmImage
  , decodeAutoNetpbmImageSequence
  ) where

import Control.Monad (guard)
import Data.Bifunctor (first)
import qualified Data.ByteString as B (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Typeable
import qualified Data.Vector.Storable as V
import Graphics.Netpbm as Netpbm hiding (PPM)
import qualified Graphics.Netpbm as Netpbm (PPM(..))
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Prelude as P

-- | Netpbm: portable bitmap image with @.pbm@ extension.
data PBM = PBM deriving Show

instance FileFormat PBM where
  type Metadata PBM = Maybe B.ByteString
  ext _ = ".pbm"

instance FileFormat (Sequence PBM) where
  type WriteOptions (Sequence PBM) = WriteOptions PBM
  type Metadata (Sequence PBM) = Maybe B.ByteString
  ext _ = ext PBM


-- | Netpbm: portable graymap image with @.pgm@ extension.
data PGM = PGM deriving Show

instance FileFormat PGM where
  type Metadata PGM = Maybe B.ByteString
  ext _ = ".pgm"

instance FileFormat (Sequence PGM) where
  type WriteOptions (Sequence PGM) = WriteOptions PGM
  type Metadata (Sequence PGM) = Maybe B.ByteString
  ext _ = ext PGM


-- | Netpbm: portable pixmap image with @.ppm@ extension.
data PPM = PPM deriving Show

instance FileFormat PPM where
  type Metadata PPM = Maybe B.ByteString
  ext _ = ".ppm"
  exts _ = [".ppm", ".pnm"]

instance FileFormat (Sequence PPM) where
  type WriteOptions (Sequence PPM) = WriteOptions PPM
  type Metadata (Sequence PPM) = Maybe B.ByteString
  ext _ = ext PPM

-- | Try to decode a Netpbm image
--
-- @since 0.2.0
decodeNetpbmImage ::
     (FileFormat f, ColorModel cs e, MonadThrow m)
  => f
  -> B.ByteString
  -> m (Image S cs e, Maybe B.ByteString)
decodeNetpbmImage = decodePPM fromNetpbmImage
{-# INLINE decodeNetpbmImage #-}

-- | Try to decode a Netpbm image sequence
--
-- @since 0.2.0
decodeNetpbmImageSequence ::
     (FileFormat (Sequence f), ColorModel cs e, MonadThrow m)
  => Sequence f
  -> B.ByteString
  -> m ([Image S cs e], Maybe B.ByteString)
decodeNetpbmImageSequence = decodePPMs fromNetpbmImage
{-# INLINE decodeNetpbmImageSequence #-}

-- | Try to decode a Netpbm image, while auto converting the colorspace.
--
-- @since 0.2.0
decodeAutoNetpbmImage ::
     (FileFormat f, Manifest r (Pixel cs e), MonadThrow m, ColorSpace cs i e)
  => f
  -> B.ByteString
  -> m (Image r cs e, Maybe B.ByteString)
decodeAutoNetpbmImage = decodePPM fromNetpbmImageAuto
{-# INLINE decodeAutoNetpbmImage #-}

-- | Try to decode a Netpbm image sequence, while auto converting the colorspace.
--
-- @since 0.2.0
decodeAutoNetpbmImageSequence ::
     (FileFormat (Sequence f), Manifest r (Pixel cs e), MonadThrow m, ColorSpace cs i e)
  => Auto (Sequence f)
  -> B.ByteString
  -> m ([Image r cs e], Maybe B.ByteString)
decodeAutoNetpbmImageSequence = decodePPMs fromNetpbmImageAuto
{-# INLINE decodeAutoNetpbmImageSequence #-}

decodePPMs :: (FileFormat f, Manifest r (Pixel cs e), ColorModel cs e, MonadThrow m) =>
              (Netpbm.PPM -> Maybe (Image r cs e))
           -> f
           -> B.ByteString
           -> m ([Image r cs e], Maybe B.ByteString)
decodePPMs converter f bs =
  case parsePPM bs of
    Left err -> throwM $ DecodeError err
    Right ([], Just _) -> throwM $ DecodeError "Cannot parse PNM image"
    Right (ppms, leftOver) -> do
      imgs <- P.traverse (fromMaybeDecode f showNetpbmCS converter) ppms
      pure (imgs, leftOver)
{-# INLINE decodePPMs #-}


decodePPM :: (FileFormat f, Manifest r (Pixel cs e), ColorModel cs e, MonadThrow m) =>
             (Netpbm.PPM -> Maybe (Image r cs e))
          -> f
          -> B.ByteString
          -> m (Image r cs e, Maybe B.ByteString)
decodePPM decoder f bs =
  case parsePPM bs of
    Left err -> throwM $ DecodeError err
    Right ([], Nothing) -> throwM $ DecodeError "PNM image is empty"
    Right ([], _) -> throwM $ DecodeError "Cannot parse PNM image"
    Right (ppm:_, leftover) -> do
      img <- fromMaybeDecode f showNetpbmCS decoder ppm
      pure (img, leftover)
{-# INLINE decodePPM #-}


fromNetpbmImageUnsafe
  :: (Storable a, Storable (Pixel cs e))
  => Int -> Int -> V.Vector a -> Maybe (Image S cs e)
fromNetpbmImageUnsafe m n v = do
  guard (n * m == V.length v)
  unsafeFromStorableVectorM (Sz (m :. n)) v


showNetpbmCS :: Netpbm.PPM -> String
showNetpbmCS Netpbm.PPM {ppmData} =
  case ppmData of
    PbmPixelData _      -> "Image S Y Bit"
    PgmPixelData8 _     -> "Image S Y Word8"
    PgmPixelData16 _    -> "Image S Y Word16"
    PpmPixelDataRGB8 _  -> "Image S RGB Word8"
    PpmPixelDataRGB16 _ -> "Image S RGB Word16"



instance Readable PBM (Image S CM.X Bit) where
  decodeWithMetadataM = decodeNetpbmImage
instance Readable PBM (Image S (Y D65) Bit) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f


instance Readable (Sequence PBM) [Image S CM.X Bit] where
  decodeWithMetadataM = decodePPMs fromNetpbmImage
instance Readable (Sequence PBM) [Image S (Y D65) Bit] where
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f


instance Readable PGM (Image S CM.X Word8) where
  decodeWithMetadataM = decodeNetpbmImage
instance Readable PGM (Image S CM.X Word16) where
  decodeWithMetadataM = decodeNetpbmImage


instance Readable PGM (Image S (Y' SRGB) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f
instance Readable PGM (Image S (Y' SRGB) Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f
instance Readable PGM (Image S (Y D65) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f
instance Readable PGM (Image S (Y D65) Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f


instance Readable (Sequence PGM) [Image S CM.X Word8] where
  decodeWithMetadataM = decodePPMs fromNetpbmImage
instance Readable (Sequence PGM) [Image S CM.X Word16] where
  decodeWithMetadataM = decodePPMs fromNetpbmImage


instance Readable (Sequence PGM) [Image S (Y' SRGB) Word8] where
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f
instance Readable (Sequence PGM) [Image S (Y' SRGB) Word16] where
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f
instance Readable (Sequence PGM) [Image S (Y D65) Word8] where
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f
instance Readable (Sequence PGM) [Image S (Y D65) Word16] where
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f


instance Readable PPM (Image S CM.RGB Word8) where
  decodeWithMetadataM = decodeNetpbmImage
instance Readable PPM (Image S CM.RGB Word16) where
  decodeWithMetadataM = decodeNetpbmImage


instance Readable PPM (Image S (SRGB 'NonLinear) Word8) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f
instance Readable PPM (Image S (SRGB 'NonLinear) Word16) where
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f


instance Readable (Sequence PPM) [Image S CM.RGB Word8] where
  decodeWithMetadataM = decodePPMs fromNetpbmImage
instance Readable (Sequence PPM) [Image S CM.RGB Word16] where
  decodeWithMetadataM = decodePPMs fromNetpbmImage


instance Readable (Sequence PPM) [Image S (SRGB 'NonLinear) Word8] where
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f
instance Readable (Sequence PPM) [Image S (SRGB 'NonLinear) Word16] where
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f


fromNetpbmImage
  :: forall cs e . ColorModel cs e =>
     Netpbm.PPM -> Maybe (Image S cs e)
fromNetpbmImage Netpbm.PPM {..} = do
  let m = ppmHeight ppmHeader
      n = ppmWidth ppmHeader
  case ppmData of
    PbmPixelData v      -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.X Bit)
                              fromNetpbmImageUnsafe m n v
    PgmPixelData8 v     -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word8)
                              fromNetpbmImageUnsafe m n v
    PgmPixelData16 v    -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word16)
                              fromNetpbmImageUnsafe m n v
    PpmPixelDataRGB8 v  -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word8)
                              fromNetpbmImageUnsafe m n v
    PpmPixelDataRGB16 v -> do Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word16)
                              fromNetpbmImageUnsafe m n v


instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto PBM) (Image r cs e) where
  decodeWithMetadataM = decodeAutoNetpbmImage
instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto (Sequence PBM)) [Image r cs e] where
  decodeWithMetadataM = decodeAutoNetpbmImageSequence

instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto PGM) (Image r cs e) where
  decodeWithMetadataM = decodeAutoNetpbmImage
instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto (Sequence PGM)) [Image r cs e] where
  decodeWithMetadataM = decodeAutoNetpbmImageSequence

instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto PPM) (Image r cs e) where
  decodeWithMetadataM = decodeAutoNetpbmImage
instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto (Sequence PPM)) [Image r cs e] where
  decodeWithMetadataM = decodeAutoNetpbmImageSequence

fromNetpbmImageAuto
  :: forall cs i e r . (Manifest r (Pixel cs e), ColorSpace cs i e) =>
     Netpbm.PPM -> Maybe (Image r cs e)
fromNetpbmImageAuto Netpbm.PPM {..} = do
  let m = ppmHeight ppmHeader
      n = ppmWidth ppmHeader
  case ppmData of
    PbmPixelData v ->
      compute . convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S (Y D65) Bit))
    PgmPixelData8 v ->
      compute . convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S (Y D65) Word8))
    PgmPixelData16 v ->
      compute . convertImage <$> (fromNetpbmImageUnsafe m n v :: Maybe (Image S (Y D65) Word16))
    PpmPixelDataRGB8 v ->
      compute . convertImage <$>
      (fromNetpbmImageUnsafe m n v :: Maybe (Image S (SRGB 'NonLinear) Word8))
    PpmPixelDataRGB16 v ->
      compute . convertImage <$>
      (fromNetpbmImageUnsafe m n v :: Maybe (Image S (SRGB 'NonLinear) Word16))
