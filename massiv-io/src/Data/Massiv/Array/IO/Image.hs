{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image
  ( module Data.Massiv.Array.IO.Image.JuicyPixels
  , module Data.Massiv.Array.IO.Image.Netpbm
  -- ** Helper image functions
  , selectFileFormat
  , Encode(..)
  , encodeImageM
  , encodeAdhocM
  , writableAdhoc
  , imageWriteFormats
  , imageWriteAutoFormats
  , Decode(..)
  , decodeImageM
  , decodeAdhocM
  , readableAdhoc
  , imageReadFormats
  , imageReadAutoFormats
  ) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.IO.Image.JuicyPixels
import Data.Massiv.Array.IO.Image.Netpbm
import Graphics.Pixel.ColorSpace
import Prelude as P



-- | Adhoc encoder
data Encode out where
  -- | Provide a custom way to encode a particular file format. This is an alternative
  -- approach to `Writable` class
  --
  -- @since 0.4.1
  Encode
    :: FileFormat f
    => f
    -> (forall m. MonadThrow m => f -> out -> m BL.ByteString)
    -> Encode out

instance Show (Encode out) where
  show (Encode f _) = show f

instance FileFormat (Encode out) where
  ext (Encode f _) = ext f

  exts (Encode f _) = exts f

-- | Decode binary data without requiring `Readable` instances
--
-- @since 0.4.1
encodeAdhocM :: MonadThrow m => Encode out -> out -> m BL.ByteString
encodeAdhocM (Encode f enc) = enc f

-- | Utilize a Writable instance in order to construct an adhoc Encode type
--
-- @since 0.4.1
writableAdhoc :: Writable f out => f -> Encode out
writableAdhoc f = Encode f (`encodeM` def)


-- | Encode an image into a lazy `BL.ByteString`, while selecting the appropriate format from the
-- file extension.
--
-- @since 0.2.0
encodeImageM
  :: MonadThrow m
  => [Encode (Image r cs e)] -- ^ List of image formats to choose from (useful lists are
                             -- `imageWriteFormats` and `imageWriteAutoFormats`)
  -> FilePath -- ^ File name with extension, so the format can be inferred
  -> Image r cs e -- ^ Image to encode
  -> m BL.ByteString
encodeImageM formats path img = do
  f <- selectFileFormat formats path
  encodeAdhocM f img


-- | List of image formats that can be encoded without any color space conversion.
imageWriteFormats :: (Source r (Pixel cs e), ColorModel cs e) => [Encode (Image r cs e)]
imageWriteFormats =
  [ Encode PNG (\ f -> encodePNG f . computeSource @S)
  , Encode TIF (\ f -> encodeTIF f . computeSource @S)
  , Encode JPG (\ f -> encodeJPG f def . computeSource @S)
  , Encode BMP (\ f -> encodeBMP f def . computeSource @S)
  , Encode GIF (\ f -> encodeGIF f def . computeSource @S)
  , Encode TGA (\ f -> encodeTGA f . computeSource @S)
  , Encode HDR (\ f -> encodeHDR f def . computeSource @S)
  ]

-- | List of image formats that can be encoded with any necessary color space conversions.
imageWriteAutoFormats ::
     (Source r (Pixel cs e), ColorSpace cs i e, ColorSpace (BaseSpace cs) i e)
  => [Encode (Image r cs e)]
imageWriteAutoFormats =
  [ Encode (Auto PNG) (\f -> pure . encodeAutoPNG f)
  , Encode (Auto TIF) (\f -> pure . encodeAutoTIF f)
  , Encode (Auto JPG) (\f -> pure . encodeAutoJPG f def)
  , Encode (Auto BMP) (\f -> pure . encodeAutoBMP f def)
  , Encode (Auto GIF) (`encodeAutoGIF` def)
  , Encode (Auto HDR) (\f -> pure . encodeAutoHDR f def)
  , Encode (Auto TGA) (\f -> pure . encodeAutoTGA f)
  ]


-- | Adhoc decoder
data Decode out where
  -- | Provide a custom way to encode a particular file format. This is an alternative
  -- approach to `Writable` class
  --
  -- @since 0.4.1
  Decode
    :: FileFormat f
    => f
    -> (forall m. MonadThrow m => f -> B.ByteString -> m out)
    -> Decode out

instance Show (Decode out) where
  show (Decode f _) = show f

instance FileFormat (Decode (Image r cs e)) where
  ext (Decode f _) = ext f

  exts (Decode f _) = exts f

-- | Decode binary data without requiring `Readable` instances
--
-- @since 0.4.1
decodeAdhocM :: MonadThrow m => Decode out -> B.ByteString -> m out
decodeAdhocM (Decode f dec) = dec f


-- | Utilize a Readable instance in order to construct an adhoc Decode type
--
-- @since 0.4.1
readableAdhoc :: Readable f out => f -> Decode out
readableAdhoc f = Decode f decodeM


-- | Decode an image from the strict `ByteString` while inferring format the image is encoded in
-- from the file extension
--
-- @since 0.2.0
decodeImageM
  :: MonadThrow m
  => [Decode (Image r cs e)] -- ^ List of available formats to choose from
  -> FilePath -- ^ File name with extension, so format can be inferred
  -> B.ByteString -- ^ Encoded image
  -> m (Image r cs e)
decodeImageM formats path bs = do
  f <- selectFileFormat formats path
  decodeAdhocM f bs

-- | List of image formats decodable with no color space conversion
imageReadFormats :: ColorModel cs e => [Decode (Image S cs e)]
imageReadFormats =
  [ Decode PNG decodePNG
  , Decode TIF decodeTIF
  , Decode JPG decodeJPG
  , Decode BMP decodeBMP
  , Decode GIF decodeGIF
  , Decode HDR decodeHDR
  , Decode TGA decodeTGA
  , Decode PBM (\f -> fmap fst . decodeNetpbmImage f)
  , Decode PGM (\f -> fmap fst . decodeNetpbmImage f)
  , Decode PPM (\f -> fmap fst . decodeNetpbmImage f)
  ]

-- | List of image formats decodable with automatic colorspace conversion
imageReadAutoFormats
  :: (Manifest r (Pixel cs e), ColorSpace cs i e)
  => [Decode (Image r cs e)]
imageReadAutoFormats =
  [ Decode (Auto PNG) decodeAutoPNG
  , Decode (Auto TIF) decodeAutoTIF
  , Decode (Auto JPG) decodeAutoJPG
  , Decode (Auto BMP) decodeAutoBMP
  , Decode (Auto GIF) decodeAutoGIF
  , Decode (Auto HDR) decodeAutoHDR
  , Decode (Auto TGA) decodeAutoTGA
  , Decode (Auto PBM) (\f -> fmap fst . decodeAutoNetpbmImage f)
  , Decode (Auto PGM) (\f -> fmap fst . decodeAutoNetpbmImage f)
  , Decode (Auto PPM) (\f -> fmap fst . decodeAutoNetpbmImage f)
  ]


