{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.IO.Image.JuicyPixels.GIF
-- Copyright   : (c) Alexey Kuleshevich 2019-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.IO.Image.JuicyPixels.GIF
  ( GIF(..)
  , GifOptions(..)
  , SequenceGifOptions(..)
  , JP.GifDelay
  , JP.GifLooping(..)
  , JP.PaletteOptions(..)
  , JP.PaletteCreationMethod(..)
  , JP.GifDisposalMethod(..)

  , decodeGIF
  , decodeWithMetadataGIF
  , decodeAutoGIF
  , decodeAutoWithMetadataGIF
  , encodeGIF
  , encodeAutoGIF
  -- Sequence
  , decodeSequenceGIF
  , decodeSequenceWithMetadataGIF
  , decodeAutoSequenceGIF
  , decodeAutoSequenceWithMetadataGIF
  ) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.ColorQuant as JP
import qualified Codec.Picture.Gif as JP
import qualified Codec.Picture.Metadata as JP
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Massiv.Array as A
import Data.Massiv.Array.IO.Base
import Data.Massiv.Array.IO.Image.JuicyPixels.Base
import Data.Typeable
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Prelude as P
import Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- GIF Format ------------------------------------------------------------------
--------------------------------------------------------------------------------


newtype GifOptions = GifOptions
  { gifPaletteOptions :: JP.PaletteOptions
  }

instance Default GifOptions where
  def = GifOptions JP.defaultPaletteOptions


-- | Graphics Interchange Format image with @.gif@ extension.
data GIF = GIF deriving Show

instance FileFormat GIF where
  type WriteOptions GIF = GifOptions
  type Metadata GIF = JP.Metadatas
  ext _ = ".gif"

instance Writable GIF (Image A.S CM.X Bit) where
  encodeM f opts img = encodeM f opts (coerceBinaryImage img)

instance Writable GIF (Image S CM.X Word8) where
  encodeM GIF _ =  pure . JP.encodeGifImage . toJPImageY8

instance Writable GIF (Image S CM.RGB Word8) where
  encodeM GIF = encodePalettizedRGB

instance Writable GIF (Image S (Y' SRGB) Word8) where
  encodeM GIF opts = encodeM GIF opts . toImageBaseModel

instance Writable GIF (Image S (Y D65) Word8) where
  encodeM GIF opts = encodeM GIF opts . toImageBaseModel

instance Writable GIF (Image S (SRGB 'NonLinear) Word8) where
  encodeM GIF opts = encodeM GIF opts . toImageBaseModel

encodePalettizedRGB ::
     (MonadThrow m, Source r (Pixel CM.RGB Word8))
  => GifOptions
  -> Image r CM.RGB Word8
  -> m BL.ByteString
encodePalettizedRGB GifOptions {gifPaletteOptions} =
  encodeError .
  uncurry JP.encodeGifImageWithPalette . JP.palettize gifPaletteOptions . toJPImageRGB8

instance (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, Source r (Pixel cs e)) =>
         Writable (Auto GIF) (Image r cs e) where
  encodeM = encodeAutoGIF


instance Readable GIF (Image S CM.RGB Word8) where
  decodeM = decodeGIF
  decodeWithMetadataM = decodeWithMetadataGIF

instance Readable GIF (Image S (Alpha CM.RGB) Word8) where
  decodeM = decodeGIF
  decodeWithMetadataM = decodeWithMetadataGIF

instance Readable GIF (Image S (SRGB 'NonLinear) Word8) where
  decodeM f = fmap fromImageBaseModel . decodeM f
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

instance Readable GIF (Image S (Alpha (SRGB 'NonLinear)) Word8) where
  decodeM f = fmap fromImageBaseModel . decodeM f
  decodeWithMetadataM f = fmap (first fromImageBaseModel) . decodeWithMetadataM f

-- | Decode a Gif Image
decodeGIF :: (ColorModel cs e, MonadThrow m) => GIF -> B.ByteString -> m (Image S cs e)
decodeGIF f bs = convertWith f (JP.decodeGif bs)

-- | Decode a Gif Image
decodeWithMetadataGIF ::
     (ColorModel cs e, MonadThrow m) => GIF -> B.ByteString -> m (Image S cs e, JP.Metadatas)
decodeWithMetadataGIF f bs = convertWithMetadata f (JP.decodeGifWithMetadata bs)


-- | Decode a Gif Image
decodeAutoGIF ::
     (Manifest r (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto GIF
  -> B.ByteString
  -> m (Image r cs e)
decodeAutoGIF f bs = convertAutoWith f (JP.decodeGif bs)

-- | Decode a Gif Image
decodeAutoWithMetadataGIF ::
     (Manifest r (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto GIF
  -> B.ByteString
  -> m (Image r cs e, JP.Metadatas)
decodeAutoWithMetadataGIF f bs = convertAutoWithMetadata f (JP.decodeGifWithMetadata bs)


instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto GIF) (Image r cs e) where
  decodeM = decodeAutoGIF
  decodeWithMetadataM = decodeAutoWithMetadataGIF

encodeGIF ::
     forall cs e m. (ColorModel cs e, MonadThrow m)
  => GIF
  -> GifOptions
  -> Image S cs e
  -> m BL.ByteString
encodeGIF f opts img =
  fallbackEncodePalettizedRGB $ do
    Refl <- eqT :: Maybe (e :~: Word8)
    JP.encodeGifImage <$> maybeJPImageY8 img
  where
    fallbackEncodePalettizedRGB =
      \case
        Just bs -> pure bs
        Nothing
          | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.X Bit)) ->
            encodeM f opts img
          | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel CM.RGB Word8)) ->
            encodePalettizedRGB opts img
          | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (SRGB 'NonLinear) Word8)) ->
            encodePalettizedRGB opts $ toImageBaseModel img
          | Just Refl <- (eqT :: Maybe (Pixel cs e :~: Pixel (AdobeRGB 'NonLinear) Word8)) ->
            encodePalettizedRGB opts $ toImageBaseModel img
        Nothing -> fromMaybeEncode f (Proxy :: Proxy (Image S cs e)) Nothing


encodeAutoGIF ::
     forall r cs i e m. (ColorSpace cs i e, Source r (Pixel cs e), MonadThrow m)
  => Auto GIF
  -> GifOptions
  -> Image r cs e
  -> m BL.ByteString
encodeAutoGIF _ opts img =
  fallbackEncodePalettizedRGB $ do
    Refl <- eqT :: Maybe (BaseModel cs :~: CM.X)
    pure $ JP.encodeGifImage $ toJPImageY8 $ A.map (toPixel8 . toPixelBaseModel) img
  where
    fallbackEncodePalettizedRGB =
      \case
        Just bs -> pure bs
        Nothing -> encodePalettizedRGB opts $ A.map toSRGB8 img

data SequenceGifOptions = SequenceGifOptions
  { sequenceGifPaletteOptions :: !JP.PaletteOptions
   -- ^ Options used for palletization. Ignored for images with Alpha channel
  , sequenceGifLooping        :: !JP.GifLooping
  }

instance Default SequenceGifOptions where
  def =
    SequenceGifOptions
      {sequenceGifPaletteOptions = JP.defaultPaletteOptions, sequenceGifLooping = JP.LoopingNever}

instance FileFormat (Sequence GIF) where
  type WriteOptions (Sequence GIF) = SequenceGifOptions
  type Metadata (Sequence GIF) = [JP.GifDelay]
  ext _ = ext GIF

instance Readable (Sequence GIF) [Image S CM.RGB Word8] where
  decodeM = decodeSequenceGIF
  decodeWithMetadataM = decodeSequenceWithMetadataGIF

instance Readable (Sequence GIF) [Image S (Alpha CM.RGB) Word8] where
  decodeM = decodeSequenceGIF
  decodeWithMetadataM = decodeSequenceWithMetadataGIF

instance Readable (Sequence GIF) [Image S (SRGB 'NonLinear) Word8] where
  decodeM f = fmap (fmap fromImageBaseModel) . decodeM f
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f

instance Readable (Sequence GIF) [Image S (Alpha (SRGB 'NonLinear)) Word8] where
  decodeM f = fmap (fmap fromImageBaseModel) . decodeM f
  decodeWithMetadataM f = fmap (first (fmap fromImageBaseModel)) . decodeWithMetadataM f


instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Readable (Auto (Sequence GIF)) [Image r cs e] where
  decodeM = decodeAutoSequenceGIF
  decodeWithMetadataM = decodeAutoSequenceWithMetadataGIF


-- | Decode a sequence of Gif images
decodeSequenceGIF ::
     (ColorModel cs e, MonadThrow m) => Sequence GIF -> B.ByteString -> m [Image S cs e]
decodeSequenceGIF f bs = convertSequenceWith f (JP.decodeGifImages bs)

-- | Decode a sequence of Gif images
decodeSequenceWithMetadataGIF ::
     (ColorModel cs e, MonadThrow m)
  => Sequence GIF
  -> B.ByteString
  -> m ([Image S cs e], [JP.GifDelay])
decodeSequenceWithMetadataGIF = decodeSeqMetadata decodeSequenceGIF

-- | Decode a sequence of Gif images
decodeAutoSequenceGIF ::
     (Manifest r (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto (Sequence GIF)
  -> B.ByteString
  -> m [Image r cs e]
decodeAutoSequenceGIF f bs = convertAutoSequenceWith f (JP.decodeGifImages bs)

-- | Decode a sequence of Gif images
decodeAutoSequenceWithMetadataGIF ::
     (Manifest r (Pixel cs e), ColorSpace cs i e, MonadThrow m)
  => Auto (Sequence GIF)
  -> B.ByteString
  -> m ([Image r cs e], [JP.GifDelay])
decodeAutoSequenceWithMetadataGIF = decodeSeqMetadata decodeAutoSequenceGIF

decodeSeqMetadata ::
     MonadThrow m => (t -> B.ByteString -> m a) -> t -> B.ByteString -> m (a, [JP.GifDelay])
decodeSeqMetadata decode f bs = do
  imgs <- decode f bs
  delays <- decodeError $ JP.getDelaysGifImages bs
  pure (imgs, delays)

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S CM.X Bit)) where
  encodeM f opts imgs = encodeM f opts (fmap (fmap coerceBinaryImage) imgs)

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S CM.X Word8)) where
  encodeM _ SequenceGifOptions {sequenceGifLooping} gifs =
    encodeError $
    JP.encodeComplexGifImage $
    JP.GifEncode
      { JP.geWidth = cols
      , JP.geHeight = rows
      , JP.gePalette = Just JP.greyPalette
      , JP.geBackground = Nothing
      , JP.geLooping = sequenceGifLooping
      , JP.geFrames =
          flip fmap (NE.toList gifs) $ \(gifDelay, gif) ->
            JP.GifFrame
              { JP.gfXOffset = 0
              , JP.gfYOffset = 0
              , JP.gfPalette = Nothing
              , JP.gfTransparent = Nothing
              , JP.gfDelay = gifDelay
              , JP.gfDisposal = JP.DisposalAny
              , JP.gfPixels = toJPImageY8 gif
              }
      }
    where
      (rows :. cols) = foldl1 (liftIndex2 max) $ fmap (unSz . size . snd) gifs

instance Writable (Sequence GIF) (NE.NonEmpty ( JP.GifDelay
                                              , JP.GifDisposalMethod
                                              , Image S CM.RGB Word8)) where
  encodeM _ SequenceGifOptions {sequenceGifLooping, sequenceGifPaletteOptions} gifs =
    encodeError $
    JP.encodeComplexGifImage $
    JP.GifEncode
      { JP.geWidth = cols
      , JP.geHeight = rows
      , JP.gePalette = Nothing
      , JP.geBackground = Nothing
      , JP.geLooping = sequenceGifLooping
      , JP.geFrames =
          flip fmap (NE.toList gifs) $ \(gifDelay, disposalMethod, gif) ->
            let (img, palette) = JP.palettize sequenceGifPaletteOptions $ toJPImageRGB8 gif
             in JP.GifFrame
                  { JP.gfXOffset = 0
                  , JP.gfYOffset = 0
                  , JP.gfPalette = Just palette
                  , JP.gfTransparent = Nothing
                  , JP.gfDelay = gifDelay
                  , JP.gfDisposal = disposalMethod
                  , JP.gfPixels = img
                  }
      }
    where
      (rows :. cols) = foldl1 (liftIndex2 max) $ fmap (\(_, _, i) -> unSz $ size i) gifs

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S CM.RGB Word8)) where
  encodeM f opts = encodeM f opts . fmap (\(d, i) -> (d, JP.DisposalAny, i))

instance Writable (Sequence GIF) (NE.NonEmpty ( JP.GifDelay
                                              , JP.GifDisposalMethod
                                              , Image S (Alpha CM.RGB) Word8)) where
  encodeM _ SequenceGifOptions {sequenceGifLooping} gifsNE =
    encodeError $
    JP.encodeComplexGifImage $
    JP.GifEncode
      { JP.geWidth = cols
      , JP.geHeight = rows
      , JP.gePalette = Nothing
      , JP.geBackground = Nothing
      , JP.geLooping = sequenceGifLooping
      , JP.geFrames =
          P.zipWith (\d f -> f {JP.gfDisposal = d}) disposals $
          JP.palettizeWithAlpha (P.zip delays $ P.map toJPImageRGBA8 images) JP.DisposalAny
      }
    where
      (delays, disposals, images) = P.unzip3 $ NE.toList gifsNE
      (rows :. cols) = foldl1 (liftIndex2 max) $ fmap (unSz . size) images

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S (Alpha CM.RGB) Word8)) where
  encodeM f opts = encodeM f opts . fmap (\(d, i) -> (d, JP.DisposalRestoreBackground, i))

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S (Y' SRGB) Word8)) where
  encodeM f opts = encodeM f opts . fmap (fmap toImageBaseModel)

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S (Y D65) Word8)) where
  encodeM f opts = encodeM f opts . fmap (fmap toImageBaseModel)

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S (SRGB 'NonLinear) Word8)) where
  encodeM f opts = encodeM f opts . fmap (fmap toImageBaseModel)

instance Writable (Sequence GIF) (NE.NonEmpty (JP.GifDelay, Image S (Alpha (SRGB 'NonLinear)) Word8)) where
  encodeM f opts = encodeM f opts . fmap (fmap toImageBaseModel)

instance Writable (Sequence GIF) (NE.NonEmpty ( JP.GifDelay
                                              , JP.GifDisposalMethod
                                              , Image S (SRGB 'NonLinear) Word8)) where
  encodeM f opts = encodeM f opts . fmap (\(dl, dp, i) -> (dl, dp, toImageBaseModel i))

instance Writable (Sequence GIF) (NE.NonEmpty ( JP.GifDelay
                                              , JP.GifDisposalMethod
                                              , Image S (Alpha (SRGB 'NonLinear)) Word8)) where
  encodeM f opts = encodeM f opts . fmap (\(dl, dp, i) -> (dl, dp, toImageBaseModel i))


instance (Manifest r (Pixel cs e), ColorSpace cs i e) =>
         Writable (Auto (Sequence GIF)) (NE.NonEmpty (JP.GifDelay, Image r cs e)) where
  encodeM (Auto f) opts =
    encodeM f opts .
    fmap (fmap (computeAs S . (convertImage :: Image r cs e -> Image D (SRGB 'NonLinear) Word8)))
