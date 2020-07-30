{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Base
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Base
  ( FileFormat(..)
  , Readable(..)
  , decode'
  , Writable(..)
  , encode'
  , ConvertError(..)
  , EncodeError(..)
  , DecodeError(..)
  , Sequence(..)
  , Auto(..)
  , Image
  , convertImage
  , toImageBaseModel
  , fromImageBaseModel
  , demoteLumaImage
  , promoteLumaImage
  , demoteLumaAlphaImage
  , promoteLumaAlphaImage
  , defaultWriteOptions
  , encodeError
  , decodeError
  , toProxy
  , fromMaybeEncode
  , fromMaybeDecodeM
  , fromMaybeDecode
  , convertEither
  , unsafeFromStorableVectorM
  , MonadThrow(..)
  ) where

import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Default.Class (Default(..))
import qualified Data.Massiv.Array as A
import Data.Typeable
import qualified Data.Vector.Storable as V
import Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Unsafe.Coerce
#if !MIN_VERSION_massiv(0,5,0)
import Data.Massiv.Array.Manifest.Vector
#endif
type Image r cs e = A.Array r A.Ix2 (Pixel cs e)

-- | Conversion error, which is thrown when there is a mismatch between the
-- expected array type and the one supported by the file format. It is also
-- thrown upon a failure of automatic conversion between those types, in case
-- such conversion is utilized.
newtype ConvertError = ConvertError String deriving Show

instance Exception ConvertError

-- | This exception can be thrown while reading/decoding a file and indicates an
-- error in the file itself.
newtype DecodeError = DecodeError String deriving Show

instance Exception DecodeError

-- | This exception can be thrown while writing/encoding into a file and
-- indicates an error in an array that is being encoded.
newtype EncodeError = EncodeError String deriving Show

instance Exception EncodeError


-- | Generate default write options for a file format
defaultWriteOptions :: FileFormat f => f -> WriteOptions f
defaultWriteOptions _ = def


-- | Special wrapper for formats that support encoding/decoding sequence of array.
newtype Sequence f = Sequence f deriving Show

newtype Auto f = Auto f deriving Show

-- | File format. Helps in guessing file format from a file extension,
-- as well as supplying format specific options during saving the file.
class (Default (WriteOptions f), Show f) => FileFormat f where
  -- | Options that can be used during writing a file in this format.
  type WriteOptions f
  type WriteOptions f = ()

  type Metadata f
  type Metadata f = ()

  -- | Default file extension for this file format.
  ext :: f -> String

  -- | Other known file extensions for this file format, eg. ".jpeg", ".jpg".
  exts :: f -> [String]
  exts f = [ext f]

  -- | Checks if a file extension corresponds to the format, eg.
  -- @isFormat ".png" PNG == True@
  isFormat :: String -> f -> Bool
  isFormat e f = e `elem` exts f


instance FileFormat f => FileFormat (Auto f) where
  type WriteOptions (Auto f) = WriteOptions f
  type Metadata (Auto f) = Metadata f

  ext (Auto f) = ext f
  exts (Auto f) = exts f


-- | File formats that can be read into arrays.
class FileFormat f => Readable f arr where
  {-# MINIMAL (decodeM | decodeWithMetadataM) #-}
  -- | Decode a `B.ByteString` into an array. Can also return whatever left over data that
  -- was not consumed during decoding.
  --
  -- @since 0.2.0
  decodeM :: MonadThrow m => f -> B.ByteString -> m arr
  decodeM f bs = fst <$> decodeWithMetadataM f bs
  -- | Just as `decodeM`, but also return any format type specific metadata
  --
  -- @since 0.2.0
  decodeWithMetadataM :: MonadThrow m => f -> B.ByteString -> m (arr, Metadata f)
  default decodeWithMetadataM :: (Metadata f ~ (), MonadThrow m) =>
    f -> B.ByteString -> m (arr, Metadata f)
  decodeWithMetadataM f bs = do
    arr <- decodeM f bs
    pure (arr, ())

-- | Encode an array into a `BL.ByteString`.
encode' :: Writable f arr => f -> WriteOptions f -> arr -> BL.ByteString
encode' f opts = either throw id . encodeM f opts

-- | Decode a `B.ByteString` into an Array.
decode' :: Readable f arr => f -> B.ByteString -> arr
decode' f = either throw id . decodeM f


-- | Arrays that can be written into a file.
class FileFormat f => Writable f arr where

  -- | Encode an array into a `BL.ByteString`.
  --
  -- @since 0.2.0
  encodeM :: MonadThrow m => f -> WriteOptions f -> arr -> m BL.ByteString

-- | Helper function to create a `Proxy` from the value.
toProxy :: a -> Proxy a
toProxy _ = Proxy

showImageType ::
     forall r cs e. (Typeable r, ColorModel cs e)
  => Proxy (Image r cs e)
  -> String
showImageType _ =
  ("<Image " ++) .
  showsTypeRep (typeRep (Proxy :: Proxy r)) .
  (' ' :) .
  showsColorModelName (Proxy :: Proxy (Color cs e)) .
  (' ' :) . showsTypeRep (typeRep (Proxy :: Proxy e)) $
  ">"


-- | Encode an image using the supplied function or throw an error in case of failure.
fromMaybeEncode
  :: forall f r cs e b m. (ColorModel cs e, FileFormat f, Typeable r, MonadThrow m)
  => f -> Proxy (Image r cs e) -> Maybe b -> m b
fromMaybeEncode f imgProxy =
  \case
    Just b -> pure b
    Nothing ->
      throwM $
      ConvertError ("Format " ++ show f ++ " cannot be encoded as " ++ showImageType imgProxy)


-- | Decode an image using the supplied function or throw an error in case of failure.
fromMaybeDecode ::
     forall r cs e a f m. (ColorModel cs e, FileFormat f, Typeable r, MonadThrow m)
  => f
  -> (a -> String)
  -> (a -> Maybe (Image r cs e))
  -> a
  -> m (Image r cs e)
fromMaybeDecode f showCS conv eImg =
  case conv eImg of
    Nothing ->
      throwM $
      ConvertError $
      "Cannot decode " ++
      show f ++
      " image <" ++ showCS eImg ++ "> as " ++ showImageType (Proxy :: Proxy (Image r cs e))
    Just img -> pure img

-- | Decode an image using the supplied function or throw an error in case of failure.
fromMaybeDecodeM ::
     forall r cs e a f m. (ColorModel cs e, FileFormat f, Typeable r, MonadThrow m)
  => f
  -> (a -> String)
  -> (a -> m (Maybe (Image r cs e)))
  -> a
  -> m (Image r cs e)
fromMaybeDecodeM f showCS conv eImg =
  conv eImg >>= \case
    Nothing ->
      throwM $
      ConvertError $
      "Cannot decode " ++
      show f ++
      " image <" ++ showCS eImg ++ "> as " ++ showImageType (Proxy :: Proxy (Image r cs e))
    Just img -> pure img


-- | Convert an image using the supplied function and return ConvertError error in case of failure.
convertEither ::
     forall r cs i e a f m. (ColorSpace cs i e, FileFormat f, Typeable r, MonadThrow m)
  => f
  -> (a -> String)
  -> (a -> Maybe (Image r cs e))
  -> a
  -> m (Image r cs e)
convertEither f showCS conv eImg =
  maybe
    (throwM $
     ConvertError
       ("Cannot convert " ++
        show f ++
        " image <" ++ showCS eImg ++ "> as " ++ showImageType (Proxy :: Proxy (Image r cs e))))
    pure
    (conv eImg)


encodeError :: MonadThrow m => Either String a -> m a
encodeError = either (throwM . EncodeError) pure

decodeError :: MonadThrow m => Either String a -> m a
decodeError = either (throwM . DecodeError) pure


-- | Convert image to any supported color space
--
-- @since 0.2.0
convertImage ::
     (A.Source r' A.Ix2 (Pixel cs' e'), ColorSpace cs' i' e', ColorSpace cs i e)
  => Image r' cs' e'
  -> Image A.D cs e
convertImage = A.map convertPixel

-- | Cast an array. This is theoretically unsafe operation, but for all currently
-- available `ColorSpace` instances this function is perfectly safe.
--
-- @since 0.2.0
toImageBaseModel :: A.Array A.S A.Ix2 (Pixel cs e) -> A.Array A.S A.Ix2 (Pixel (BaseModel cs) e)
toImageBaseModel = unsafeCoerce


-- | Cast an array. This is theoretically unsafe operation, but for all currently
-- available `ColorSpace` instances this function is perfectly safe.
--
-- @since 0.2.0
fromImageBaseModel :: A.Array A.S A.Ix2 (Pixel (BaseModel cs) e) -> A.Array A.S A.Ix2 (Pixel cs e)
fromImageBaseModel = unsafeCoerce


-- | Cast an array with Luma pixels to an array with pixels in a plain single channel
-- `CM.X` color model
--
-- @since 0.2.1
demoteLumaImage :: A.Array A.S A.Ix2 (Pixel (Y' cs) e) -> A.Array A.S A.Ix2 (Pixel CM.X e)
demoteLumaImage = unsafeCoerce
{-# DEPRECATED demoteLumaImage "In favor of `toImageBaseModel`" #-}

-- | Cast an array with pixels in a plain single channel `CM.X` color model to an array
-- with Luma pixels
--
-- @since 0.2.1
promoteLumaImage :: A.Array A.S A.Ix2 (Pixel CM.X e) -> A.Array A.S A.Ix2 (Pixel (Y' cs) e)
promoteLumaImage = unsafeCoerce
{-# DEPRECATED promoteLumaImage "In favor of `fromImageBaseModel`" #-}

-- | Same as `demoteLumaImage`, but with Alpha channel
--
-- @since 0.2.1
demoteLumaAlphaImage ::
     A.Array A.S A.Ix2 (Pixel (Alpha (Y' cs)) e) -> A.Array A.S A.Ix2 (Pixel (Alpha CM.X) e)
demoteLumaAlphaImage = unsafeCoerce
{-# DEPRECATED demoteLumaAlphaImage "In favor of `toImageBaseModel`" #-}


-- | Same as `promoteLumaImage` but with Alpha channel
--
-- @since 0.2.1
promoteLumaAlphaImage ::
     A.Array A.S A.Ix2 (Pixel (Alpha CM.X) e) -> A.Array A.S A.Ix2 (Pixel (Alpha (Y' cs)) e)
promoteLumaAlphaImage = unsafeCoerce
{-# DEPRECATED promoteLumaAlphaImage "In favor of `fromImageBaseModel`" #-}



unsafeFromStorableVectorM ::
     (MonadThrow m, A.Index ix, A.Storable a, A.Storable b)
  => A.Sz ix
  -> V.Vector a
  -> m (A.Array A.S ix b)
unsafeFromStorableVectorM sz v =
#if MIN_VERSION_massiv(0,5,0)
    A.resizeM sz $ A.fromStorableVector A.Par $ V.unsafeCast v
#else
    fromVectorM A.Par sz $ V.unsafeCast v
#endif
