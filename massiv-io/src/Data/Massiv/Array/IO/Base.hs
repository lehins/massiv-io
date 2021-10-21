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
-- Copyright   : (c) Alexey Kuleshevich 2018-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Base
  ( FileFormat(..)
  , selectFileFormat
  , Readable(..)
  , decode'
  , Writable(..)
  , encode'
  , ConvertError(..)
  , EncodeError(..)
  , DecodeError(..)
  , Sequence(..)
  , Default(..)
  , Auto(..)
  , Image
  , convertImage
  , convertImageM
  , toImageBaseModel
  , fromImageBaseModel
  , coerceBinaryImage
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

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Char (toLower)
import Data.Default.Class (Default(..))
import qualified Data.Massiv.Array as A
import Data.Typeable
import qualified Data.Vector.Storable as V
import GHC.Stack
import Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Prelude as P
import System.FilePath (takeExtension)
import Unsafe.Coerce

type Image r cs e = A.Matrix r (Pixel cs e)

-- | Conversion error, which is thrown when there is a mismatch between the expected array
-- type and the one supported by the file format. It is also thrown upon a failure of
-- automatic conversion between those types, in case when such conversion is utilized.
newtype ConvertError = ConvertError String deriving Show

instance Exception ConvertError where
  displayException (ConvertError str) = "ConvertError: " ++ str

-- | This exception can be thrown while reading/decoding a file and indicates an
-- error in the file itself.
newtype DecodeError = DecodeError String deriving Show

instance Exception DecodeError where
  displayException (DecodeError str) = "DecodeError: " ++ str

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

-- | Try to select a file format by looking at the file extension and matching it to one
-- of the formats in the list
--
-- @since 0.4.1
selectFileFormat :: (FileFormat f, MonadThrow m) => [f] -> FilePath -> m f
selectFileFormat formats path = do
  let ext' = P.map toLower . takeExtension $ path
  case P.dropWhile (not . isFormat ext') formats of
    []    -> throwM $ EncodeError $ "File format is not supported: " ++ ext'
    (f:_) -> pure f


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

-- | Encode an array into a lazy `BL.ByteString`.
encode' :: (Writable f arr, HasCallStack) => f -> WriteOptions f -> arr -> BL.ByteString
encode' f opts = A.throwEither . encodeM f opts

-- | Decode a strict `B.ByteString` into an Array.
decode' :: (Readable f arr, HasCallStack) => f -> B.ByteString -> arr
decode' f = A.throwEither . decodeM f


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
     (A.Source r' (Pixel cs' e'), ColorSpace cs' i' e', ColorSpace cs i e)
  => Image r' cs' e'
  -> Image A.D cs e
convertImage = A.map convertPixel
{-# INLINE convertImage #-}


-- | Convert image to any supported color space and compute the resulting image
--
-- @since 1.0.1
convertImageM ::
     (ColorSpace cs' i' e', ColorSpace cs i e, A.Manifest r (Pixel cs e))
  => Image A.S cs' e'
  -> Image r cs e
convertImageM = A.compute . A.map convertPixel
{-# INLINE [1] convertImageM #-}
{-# RULES
 "convertImageM = id" convertImageM = id
#-}

-- | Cast an array. This is theoretically unsafe operation, but for all currently
-- available `ColorSpace` instances this function is perfectly safe.
--
-- @since 0.2.0
toImageBaseModel :: A.Matrix A.S (Pixel cs e) -> A.Matrix A.S (Pixel (BaseModel cs) e)
toImageBaseModel = unsafeCoerce


-- | Cast an array. This is theoretically unsafe operation, but for all currently
-- available `ColorSpace` instances this function is perfectly safe.
--
-- @since 0.2.0
fromImageBaseModel :: A.Matrix A.S (Pixel (BaseModel cs) e) -> A.Matrix A.S (Pixel cs e)
fromImageBaseModel = unsafeCoerce

-- | Convert Binary image to its Word8 backed pixel without copy
--
-- @since 0.4.1
coerceBinaryImage :: A.Matrix A.S (Pixel CM.X Bit) -> A.Matrix A.S (Pixel CM.X Word8)
coerceBinaryImage = unsafeCoerce

unsafeFromStorableVectorM ::
     (MonadThrow m, A.Index ix, A.Storable a, A.Storable b)
  => A.Sz ix
  -> V.Vector a
  -> m (A.Array A.S ix b)
unsafeFromStorableVectorM sz v =
  A.resizeM sz $ A.fromStorableVector A.Par $ V.unsafeCast v
