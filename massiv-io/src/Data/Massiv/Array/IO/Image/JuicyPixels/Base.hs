{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.Base
-- Copyright   : (c) Alexey Kuleshevich 2019-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.Base
  ( showJP
  , convertWith
  , convertWithMetadata
  , convertAutoWith
  , convertAutoWithMetadata
  , convertSequenceWith
  , convertAutoSequenceWith
  , toJPImageY8
  , toJPImageY16
  , toJPImageY32
  , toJPImageYA8
  , toJPImageYA16
  , toJPImageYF
  , toJPImageRGB8
  , toJPImageRGB16
  , toJPImageRGBA8
  , toJPImageRGBA16
  , toJPImageRGBF
  , toJPImageYCbCr8
  , toJPImageCMYK8
  , toJPImageCMYK16
  , maybeJPImageY8
  , maybeJPImageY16
  , maybeJPImageY32
  , maybeJPImageYA8
  , maybeJPImageYA16
  , maybeJPImageYF
  , maybeJPImageRGB8
  , maybeJPImageRGB16
  , maybeJPImageRGBA8
  , maybeJPImageRGBA16
  , maybeJPImageRGBF
  , maybeJPImageYCbCr8
  , maybeJPImageCMYK8
  , maybeJPImageCMYK16
  , fromDynamicImage
  , fromDynamicImageM
  , fromDynamicImageAuto
  -- * Conversion to sRGB
  , toYCbCr8
  , toCMYK8
  , toCMYK16
  , toSRGB8
  , toSRGB16
  , toSRGBA8
  , toSRGBA16
  ) where

import qualified Codec.Picture as JP
import Control.Exception (assert)
import Control.Monad (msum, unless)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Typeable
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(sizeOf))
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Prelude as P

--------------------------------------------------------------------------------
-- Common JuciyPixels encoding/decoding functions ------------------------------
--------------------------------------------------------------------------------

convertWith ::
     (MonadThrow m, ColorModel cs e, FileFormat f)
  => f
  -> Either String JP.DynamicImage
  -> m (Image S cs e)
convertWith f = either (throwM . DecodeError) (fromMaybeDecodeM f showJP fromDynamicImageM)


convertWithMetadata ::
     (MonadThrow m, ColorModel cs e, FileFormat f)
  => f
  -> Either String (JP.DynamicImage, Metadata f)
  -> m (Image S cs e, Metadata f)
convertWithMetadata f decoded =
  case decoded of
    Left err -> throwM $ DecodeError err
    Right (jp, meta) -> do
      i <- fromMaybeDecodeM f showJP fromDynamicImageM jp
      pure (i, meta)

convertAutoWithMetadata ::
     (MonadThrow m, Manifest r (Pixel cs e), ColorSpace cs i e)
  => Auto f
  -> Either String (JP.DynamicImage, Metadata f)
  -> m (Image r cs e, Metadata f)
convertAutoWithMetadata _ decoded =
  case decoded of
    Left err -> throwM $ DecodeError err
    Right (jp, meta) -> do
      i <- fromDynamicImageAuto jp
      pure (i, meta)

convertAutoWith ::
     (MonadThrow m, Manifest r (Pixel cs e), ColorSpace cs i e)
  => Auto f
  -> Either String JP.DynamicImage
  -> m (Image r cs e)
convertAutoWith _ = either (throwM . DecodeError) fromDynamicImageAuto


convertSequenceWith ::
     (MonadThrow m, ColorModel cs e, FileFormat (Sequence f))
  => Sequence f
  -> Either String [JP.DynamicImage]
  -> m [Image S cs e]
convertSequenceWith f ejpImgs = do
  jpImgs <- decodeError ejpImgs
  P.traverse (fromMaybeDecodeM f showJP fromDynamicImageM) jpImgs


convertAutoSequenceWith ::
     (MonadThrow m, Manifest r (Pixel cs e), ColorSpace cs i e)
  => Auto (Sequence f)
  -> Either String [JP.DynamicImage]
  -> m [Image r cs e]
convertAutoSequenceWith _ ejpImgs = do
  jpImgs <- decodeError ejpImgs
  P.traverse fromDynamicImageAuto jpImgs


fromDynamicImageM ::
     forall cs e m. (ColorModel cs e, MonadThrow m)
  => JP.DynamicImage
  -> m (Maybe (Image S cs e))
fromDynamicImageM jpDynImg =
  case jpDynImg of
    JP.ImageY8 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y' SRGB) Word8)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y D65) Word8)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word8)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageY16 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y' SRGB) Word16)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y D65) Word16)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word16)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageY32 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y' SRGB) Word32)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y D65) Word32)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word32)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageYF jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y' SRGB) Float)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y D65) Float)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.X Float)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageYA8 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (Y' SRGB)) Word8)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (Y D65)) Word8)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.X) Word8)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageYA16 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (Y' SRGB)) Word16)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (Y D65)) Word16)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.X) Word16)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageRGB8 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (SRGB 'NonLinear) Word8)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (AdobeRGB 'NonLinear) Word8)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word8)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageRGB16 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (SRGB 'NonLinear) Word16)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (AdobeRGB 'NonLinear) Word16)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word16)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageRGBF jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (SRGB 'NonLinear) Float)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (AdobeRGB 'NonLinear) Float)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Float)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageRGBA8 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (SRGB 'NonLinear)) Word8)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (AdobeRGB 'NonLinear)) Word8)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.RGB) Word8)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageRGBA16 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (SRGB 'NonLinear)) Word16)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha (AdobeRGB 'NonLinear)) Word16)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.RGB) Word16)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageYCbCr8 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y'CbCr SRGB) Word8)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (Y'CbCr AdobeRGB) Word8)) -> justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.YCbCr Word8)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageCMYK8 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (CMYK (SRGB 'NonLinear)) Word8)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (CMYK (AdobeRGB 'NonLinear)) Word8)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.CMYK Word8)) -> justFromJP jimg
      | otherwise -> pure Nothing
    JP.ImageCMYK16 jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (CMYK (SRGB 'NonLinear)) Word16)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (CMYK (AdobeRGB 'NonLinear)) Word16)) ->
        justFromJP jimg
      | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.CMYK Word16)) -> justFromJP jimg
      | otherwise -> pure Nothing
  where
    justFromJP :: JP.Pixel px => JP.Image px -> m (Maybe (Image S cs e))
    justFromJP = fmap Just . fromJPImageUnsafeM

fromDynamicImage ::
     forall cs e. ColorModel cs e
  => JP.DynamicImage
  -> Maybe (Image S cs e)
fromDynamicImage jpDynImg =
  case jpDynImg of
    JP.ImageY8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word8)
      fromJPImageUnsafeM jimg
    JP.ImageY16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word16)
      fromJPImageUnsafeM jimg
    JP.ImageY32 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.X Word32)
      fromJPImageUnsafeM jimg
    JP.ImageYF jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.X Float)
      fromJPImageUnsafeM jimg
    JP.ImageYA8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.X) Word8)
      fromJPImageUnsafeM jimg
    JP.ImageYA16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.X) Word16)
      fromJPImageUnsafeM jimg
    JP.ImageRGB8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word8)
      fromJPImageUnsafeM jimg
    JP.ImageRGB16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word16)
      fromJPImageUnsafeM jimg
    JP.ImageRGBF jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Float)
      fromJPImageUnsafeM jimg
    JP.ImageRGBA8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.RGB) Word8)
      fromJPImageUnsafeM jimg
    JP.ImageRGBA16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel (Alpha CM.RGB) Word16)
      fromJPImageUnsafeM jimg
    JP.ImageYCbCr8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.YCbCr Word8)
      fromJPImageUnsafeM jimg
    JP.ImageCMYK8 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.CMYK Word8)
      fromJPImageUnsafeM jimg
    JP.ImageCMYK16 jimg -> do
      Refl <- eqT :: Maybe (Pixel cs e :~: Pixel CM.CMYK Word16)
      fromJPImageUnsafeM jimg
{-# DEPRECATED fromDynamicImage "In favor of `fromDynamicImageM`" #-}

fromDynamicImageAuto ::
     forall r cs i e m. (Manifest r (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => JP.DynamicImage
  -> m (Image r cs e)
fromDynamicImageAuto jpDynImg =
  case jpDynImg of
    JP.ImageY8 jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (Y D65) Word8))
    JP.ImageY16 jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (Y D65) Word16))
    JP.ImageY32 jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (Y D65) Word32))
    JP.ImageYF jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (Y D65) Float))
    JP.ImageYA8 jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (Alpha (Y D65)) Word8))
    JP.ImageYA16 jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (Alpha (Y D65)) Word16))
    JP.ImageRGB8 jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (SRGB 'NonLinear) Word8))
    JP.ImageRGB16 jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (SRGB 'NonLinear) Word16))
    JP.ImageRGBF jimg ->
      compute . convertImage <$> (fromJPImageUnsafeM jimg :: m (Image S (SRGB 'NonLinear) Float))
    JP.ImageRGBA8 jimg ->
      compute . convertImage <$>
      (fromJPImageUnsafeM jimg :: m (Image S (Alpha (SRGB 'NonLinear)) Word8))
    JP.ImageRGBA16 jimg ->
      compute . convertImage <$>
      (fromJPImageUnsafeM jimg :: m (Image S (Alpha (SRGB 'NonLinear)) Word16))
    JP.ImageYCbCr8 jimg ->
      compute . convertImage <$>
      (fromJPImageUnsafeM jimg :: m (Image S (Y'CbCr SRGB) Word8))
    JP.ImageCMYK8 jimg ->
      compute . convertImage <$>
      (fromJPImageUnsafeM jimg :: m (Image S (CMYK (SRGB 'NonLinear)) Word8))
    JP.ImageCMYK16 jimg ->
      compute . convertImage <$>
      (fromJPImageUnsafeM jimg :: m (Image S (CMYK (SRGB 'NonLinear)) Word16))



showJP :: JP.DynamicImage -> String
showJP (JP.ImageY8     _) = "Image S Y Word8"
showJP (JP.ImageY16    _) = "Image S Y Word16"
showJP (JP.ImageY32    _) = "Image S Y Word32"
showJP (JP.ImageYF     _) = "Image S Y Float"
showJP (JP.ImageYA8    _) = "Image S YA Word8"
showJP (JP.ImageYA16   _) = "Image S YA Word16"
showJP (JP.ImageRGB8   _) = "Image S RGB Word8"
showJP (JP.ImageRGB16  _) = "Image S RGB Word16"
showJP (JP.ImageRGBF   _) = "Image S RGB Float"
showJP (JP.ImageRGBA8  _) = "Image S RGBA Word8"
showJP (JP.ImageRGBA16 _) = "Image S RGBA Word16"
showJP (JP.ImageYCbCr8 _) = "Image S YCbCr Word8"
showJP (JP.ImageCMYK8  _) = "Image S CMYK Word8"
showJP (JP.ImageCMYK16 _) = "Image S CMYK Word16"


-- Encoding

toJPImageUnsafe ::
     forall r cs a.
     ( JP.Pixel a
     , Source r (Pixel cs (JP.PixelBaseComponent a))
     , ColorModel cs (JP.PixelBaseComponent a)
     )
  => Image r cs (JP.PixelBaseComponent a)
  -> JP.Image a
toJPImageUnsafe img = JP.Image n m $ V.unsafeCast $ toStorableVector arrS
  where
    !arrS = computeSource img :: Image S cs (JP.PixelBaseComponent a)
    Sz (m :. n) = size img
{-# INLINE toJPImageUnsafe #-}

toJPImageY8 :: Source r (Pixel CM.X Word8) => Image r CM.X Word8 -> JP.Image JP.Pixel8
toJPImageY8 = toJPImageUnsafe
{-# INLINE toJPImageY8 #-}

maybeJPImageY8 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word8))
  => Image S cs Word8
  -> Maybe (JP.Image JP.Pixel8)
maybeJPImageY8 img =
  msum
    [ (\Refl -> toJPImageY8 img) <$> (eqT :: Maybe (cs :~: CM.X))
    , (\Refl -> toJPImageY8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y' SRGB))
    , (\Refl -> toJPImageY8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y D65))
    ]
{-# INLINE maybeJPImageY8 #-}

toJPImageY16 :: Source r (Pixel CM.X Word16) => Image r CM.X Word16 -> JP.Image JP.Pixel16
toJPImageY16 = toJPImageUnsafe
{-# INLINE toJPImageY16 #-}



maybeJPImageY16 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word16))
  => Image S cs Word16
  -> Maybe (JP.Image JP.Pixel16)
maybeJPImageY16 img =
  msum
    [ (\Refl -> toJPImageY16 img) <$> (eqT :: Maybe (cs :~: CM.X))
    , (\Refl -> toJPImageY16 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y' SRGB))
    , (\Refl -> toJPImageY16 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y D65))
    ]
{-# INLINE maybeJPImageY16 #-}

toJPImageY32 :: Source r (Pixel CM.X Word32) => Image r CM.X Word32 -> JP.Image JP.Pixel32
toJPImageY32 = toJPImageUnsafe
{-# INLINE toJPImageY32 #-}


maybeJPImageY32 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word32))
  => Image S cs Word32
  -> Maybe (JP.Image JP.Pixel32)
maybeJPImageY32 img =
  msum
    [ (\Refl -> toJPImageY32 img) <$> (eqT :: Maybe (cs :~: CM.X))
    , (\Refl -> toJPImageY32 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y' SRGB))
    , (\Refl -> toJPImageY32 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y D65))
    ]
{-# INLINE maybeJPImageY32 #-}


toJPImageYF :: Source r (Pixel CM.X Float) => Image r CM.X Float -> JP.Image JP.PixelF
toJPImageYF = toJPImageUnsafe
{-# INLINE toJPImageYF #-}


maybeJPImageYF ::
     forall cs. (Typeable cs, Storable (Pixel cs Float))
  => Image S cs Float
  -> Maybe (JP.Image JP.PixelF)
maybeJPImageYF img =
  msum
    [ (\Refl -> toJPImageYF img) <$> (eqT :: Maybe (cs :~: CM.X))
    , (\Refl -> toJPImageYF $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y' SRGB))
    , (\Refl -> toJPImageYF $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y D65))
    ]
{-# INLINE maybeJPImageYF #-}


toJPImageYA8 ::
     Source r (Pixel (Alpha CM.X) Word8) => Image r (Alpha CM.X) Word8 -> JP.Image JP.PixelYA8
toJPImageYA8 = toJPImageUnsafe
{-# INLINE toJPImageYA8 #-}

maybeJPImageYA8 ::
     forall cs. (Typeable cs, Storable (Pixel (Alpha cs) Word8))
  => Image S (Alpha cs) Word8
  -> Maybe (JP.Image JP.PixelYA8)
maybeJPImageYA8 img =
  msum
    [ (\Refl -> toJPImageYA8 img) <$> (eqT :: Maybe (cs :~: CM.X))
    , (\Refl -> toJPImageYA8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y' SRGB))
    , (\Refl -> toJPImageYA8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y D65))
    ]
{-# INLINE maybeJPImageYA8 #-}


toJPImageYA16 ::
     Source r (Pixel (Alpha CM.X) Word16)
  => Image r (Alpha CM.X) Word16
  -> JP.Image JP.PixelYA16
toJPImageYA16 = toJPImageUnsafe
{-# INLINE toJPImageYA16 #-}


maybeJPImageYA16 ::
     forall cs. (Typeable cs, Storable (Pixel (Alpha cs) Word16))
  => Image S (Alpha cs) Word16
  -> Maybe (JP.Image JP.PixelYA16)
maybeJPImageYA16 img =
  msum
    [ (\Refl -> toJPImageYA16 img) <$> (eqT :: Maybe (cs :~: CM.X))
    , (\Refl -> toJPImageYA16 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y' SRGB))
    , (\Refl -> toJPImageYA16 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y D65))
    ]
{-# INLINE maybeJPImageYA16 #-}



toJPImageRGB8 :: Source r (Pixel CM.RGB Word8) => Image r CM.RGB Word8 -> JP.Image JP.PixelRGB8
toJPImageRGB8 = toJPImageUnsafe
{-# INLINE toJPImageRGB8 #-}


maybeJPImageRGB8 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word8))
  => Image S cs Word8
  -> Maybe (JP.Image JP.PixelRGB8)
maybeJPImageRGB8 img =
  msum
    [ (\Refl -> toJPImageRGB8 img) <$> (eqT :: Maybe (cs :~: CM.RGB))
    , (\Refl -> toJPImageRGB8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: SRGB 'NonLinear))
    , (\Refl -> toJPImageRGB8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: AdobeRGB 'NonLinear))
    ]
{-# INLINE maybeJPImageRGB8 #-}


toJPImageRGB16 ::
     Source r (Pixel CM.RGB Word16) => Image r CM.RGB Word16 -> JP.Image JP.PixelRGB16
toJPImageRGB16 = toJPImageUnsafe
{-# INLINE toJPImageRGB16 #-}

maybeJPImageRGB16 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word16))
  => Image S cs Word16
  -> Maybe (JP.Image JP.PixelRGB16)
maybeJPImageRGB16 img =
  msum
    [ (\Refl -> toJPImageRGB16 img) <$> (eqT :: Maybe (cs :~: CM.RGB))
    , (\Refl -> toJPImageRGB16 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: SRGB 'NonLinear))
    , (\Refl -> toJPImageRGB16 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: AdobeRGB 'NonLinear))
    ]
{-# INLINE maybeJPImageRGB16 #-}


toJPImageRGBF :: Source r (Pixel CM.RGB Float) => Image r CM.RGB Float -> JP.Image JP.PixelRGBF
toJPImageRGBF = toJPImageUnsafe
{-# INLINE toJPImageRGBF #-}

maybeJPImageRGBF ::
     forall cs. (Typeable cs, Storable (Pixel cs Float))
  => Image S cs Float
  -> Maybe (JP.Image JP.PixelRGBF)
maybeJPImageRGBF img =
  msum
    [ (\Refl -> toJPImageRGBF img) <$> (eqT :: Maybe (cs :~: CM.RGB))
    , (\Refl -> toJPImageRGBF $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: SRGB 'NonLinear))
    , (\Refl -> toJPImageRGBF $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: AdobeRGB 'NonLinear))
    ]
{-# INLINE maybeJPImageRGBF #-}


toJPImageRGBA8 ::
     Source r (Pixel (Alpha CM.RGB) Word8)
  => Image r (Alpha CM.RGB) Word8
  -> JP.Image JP.PixelRGBA8
toJPImageRGBA8 = toJPImageUnsafe
{-# INLINE toJPImageRGBA8 #-}

maybeJPImageRGBA8 ::
     forall cs. (Typeable cs, Storable (Pixel (Alpha cs) Word8))
  => Image S (Alpha cs) Word8
  -> Maybe (JP.Image JP.PixelRGBA8)
maybeJPImageRGBA8 img =
  msum
    [ (\Refl -> toJPImageRGBA8 img) <$> (eqT :: Maybe (cs :~: CM.RGB))
    , (\Refl -> toJPImageRGBA8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: SRGB 'NonLinear))
    , (\Refl -> toJPImageRGBA8 $ toImageBaseModel img) <$>
      (eqT :: Maybe (cs :~: AdobeRGB 'NonLinear))
    ]
{-# INLINE maybeJPImageRGBA8 #-}


toJPImageRGBA16 ::
     Source r (Pixel (Alpha CM.RGB) Word16)
  => Image r (Alpha CM.RGB) Word16
  -> JP.Image JP.PixelRGBA16
toJPImageRGBA16 = toJPImageUnsafe
{-# INLINE toJPImageRGBA16 #-}

maybeJPImageRGBA16 ::
     forall cs. (Typeable cs, Storable (Pixel (Alpha cs) Word16))
  => Image S (Alpha cs) Word16
  -> Maybe (JP.Image JP.PixelRGBA16)
maybeJPImageRGBA16 img =
  msum
    [ (\Refl -> toJPImageRGBA16 img) <$> (eqT :: Maybe (cs :~: CM.RGB))
    , (\Refl -> toJPImageRGBA16 $ toImageBaseModel img) <$>
      (eqT :: Maybe (cs :~: SRGB 'NonLinear))
    , (\Refl -> toJPImageRGBA16 $ toImageBaseModel img) <$>
      (eqT :: Maybe (cs :~: AdobeRGB 'NonLinear))
    ]
{-# INLINE maybeJPImageRGBA16 #-}

toJPImageYCbCr8 ::
     Source r (Pixel CM.YCbCr Word8) => Image r CM.YCbCr Word8 -> JP.Image JP.PixelYCbCr8
toJPImageYCbCr8 = toJPImageUnsafe
{-# INLINE toJPImageYCbCr8 #-}


maybeJPImageYCbCr8 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word8))
  => Image S cs Word8
  -> Maybe (JP.Image JP.PixelYCbCr8)
maybeJPImageYCbCr8 img =
  msum
    [ (\Refl -> toJPImageYCbCr8 img) <$> (eqT :: Maybe (cs :~: CM.YCbCr))
    , (\Refl -> toJPImageYCbCr8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y'CbCr SRGB))
    --, (\Refl -> toJPImageYCbCr8 $ toImageBaseModel img) <$> (eqT :: Maybe (cs :~: Y'CbCr AdobeRGB)))
    ]
{-# INLINE maybeJPImageYCbCr8 #-}


toJPImageCMYK8 ::
     Source r (Pixel CM.CMYK Word8) => Image r CM.CMYK Word8 -> JP.Image JP.PixelCMYK8
toJPImageCMYK8 = toJPImageUnsafe
{-# INLINE toJPImageCMYK8 #-}


maybeJPImageCMYK8 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word8))
  => Image S cs Word8
  -> Maybe (JP.Image JP.PixelCMYK8)
maybeJPImageCMYK8 img =
  msum
    [ (\Refl -> toJPImageCMYK8 img) <$> (eqT :: Maybe (cs :~: CM.CMYK))
    , (\Refl -> toJPImageCMYK8 $ toImageBaseModel img) <$>
      (eqT :: Maybe (cs :~: CMYK (SRGB 'NonLinear)))
    , (\Refl -> toJPImageCMYK8 $ toImageBaseModel img) <$>
      (eqT :: Maybe (cs :~: CMYK (AdobeRGB 'NonLinear)))
    ]
{-# INLINE maybeJPImageCMYK8 #-}


toJPImageCMYK16 ::
     Source r (Pixel CM.CMYK Word16) => Image r CM.CMYK Word16 -> JP.Image JP.PixelCMYK16
toJPImageCMYK16 = toJPImageUnsafe
{-# INLINE toJPImageCMYK16 #-}


maybeJPImageCMYK16 ::
     forall cs. (Typeable cs, Storable (Pixel cs Word16))
  => Image S cs Word16
  -> Maybe (JP.Image JP.PixelCMYK16)
maybeJPImageCMYK16 img =
  msum
    [ (\Refl -> toJPImageCMYK16 img) <$> (eqT :: Maybe (cs :~: CM.CMYK))
    , (\Refl -> toJPImageCMYK16 $ toImageBaseModel img) <$>
      (eqT :: Maybe (cs :~: CMYK (SRGB 'NonLinear)))
    , (\Refl -> toJPImageCMYK16 $ toImageBaseModel img) <$>
      (eqT :: Maybe (cs :~: CMYK (AdobeRGB 'NonLinear)))
    ]
{-# INLINE maybeJPImageCMYK16 #-}


-- General decoding and helper functions

fromJPImageUnsafeM ::
     forall jpx cs e m. (Storable (Pixel cs e), Storable e, JP.Pixel jpx, MonadThrow m)
  => JP.Image jpx
  -> m (Image S cs e)
fromJPImageUnsafeM (JP.Image n m !v) = do
  let numberOfComponentsFromSize = sizeOf (undefined :: Pixel cs e) `div` sizeOf (undefined :: e)
      numComponentsPerPixel = JP.componentCount (undefined :: jpx)
  unless (numComponentsPerPixel == numberOfComponentsFromSize) $
    throwM $ ConvertError $
    concat
      [ "Mismatched sizes between JuicyPixels: "
      , show numComponentsPerPixel
      , " and massiv: "
      , show numberOfComponentsFromSize
      ]
  assert (n * m * numComponentsPerPixel == V.length v) $
    unsafeFromStorableVectorM (Sz (m :. n)) v


-- Conversion to sRGB color space based color models

toYCbCr8 :: forall cs i e . ColorSpace cs i e => Pixel cs e -> Pixel CM.YCbCr Word8
toYCbCr8 = toPixelBaseModel . (convertPixel :: Pixel cs e -> Pixel (Y'CbCr SRGB) Word8)

toCMYK8 :: forall cs i e . ColorSpace cs i e => Pixel cs e -> Pixel CM.CMYK Word8
toCMYK8 = toPixelBaseModel . (convertPixel :: Pixel cs e -> Pixel (CMYK (SRGB 'NonLinear)) Word8)

toCMYK16 :: forall cs i e . ColorSpace cs i e => Pixel cs e -> Pixel CM.CMYK Word16
toCMYK16 = toPixelBaseModel . (convertPixel :: Pixel cs e -> Pixel (CMYK (SRGB 'NonLinear)) Word16)

toSRGB8 :: forall cs i e . ColorSpace cs i e => Pixel cs e -> Pixel CM.RGB Word8
toSRGB8 = toPixelBaseModel . (convertPixel :: Pixel cs e -> Pixel (SRGB 'NonLinear) Word8)

toSRGB16 :: forall cs i e . ColorSpace cs i e => Pixel cs e -> Pixel CM.RGB Word16
toSRGB16 = toPixelBaseModel . (convertPixel :: Pixel cs e -> Pixel (SRGB 'NonLinear) Word16)

toSRGBA8 :: forall cs i e . ColorSpace cs i e => Pixel cs e -> Pixel (Alpha CM.RGB) Word8
toSRGBA8 = toPixelBaseModel . (convertPixel :: Pixel cs e -> Pixel (Alpha (SRGB 'NonLinear)) Word8)

toSRGBA16 :: forall cs i e . ColorSpace cs i e => Pixel cs e -> Pixel (Alpha CM.RGB) Word16
toSRGBA16 = toPixelBaseModel . (convertPixel :: Pixel cs e -> Pixel (Alpha (SRGB 'NonLinear)) Word16)
