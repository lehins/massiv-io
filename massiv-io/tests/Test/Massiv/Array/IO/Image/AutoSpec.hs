{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.IO.Image.AutoSpec (spec) where

import Data.Massiv.Array
import Data.Massiv.Array.IO
import Test.Massiv.Core
import Test.Massiv.Array.IO.Image.Common

spec :: Spec
spec =
  describe "Auto" $
  describe "Encode/Decode" $ do
    specEncodeDecodeNoErrorAuto BMP
    specEncodeDecodeNoErrorAuto GIF
    --specEncodeDecodeNoErrorAuto HDR -- Get "Invalid sanline size" from JuicyPixels
    specEncodeDecodeNoErrorAuto JPG
    specEncodeDecodeNoErrorAuto PNG
    specEncodeDecodeNoErrorAuto TGA
    specEncodeDecodeNoErrorAuto TIF


specEncodeDecodeNoErrorAuto ::
     ( Show f
     , Readable (Auto f) (Image S (Y D65) Word8)
     , Readable (Auto f) (Image S (Y D65) Word16)
     , Readable (Auto f) (Image S (Y D65) Word32)
     , Readable (Auto f) (Image S (Y D65) Word64)
     , Readable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word8)
     , Readable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word16)
     , Readable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word32)
     , Readable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word64)
     , Readable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word8)
     , Readable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word16)
     , Readable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word32)
     , Readable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word64)
     , Readable (Auto f) (Image S (Y'CbCr SRGB) Word8)
     , Readable (Auto f) (Image S (Y'CbCr SRGB) Word16)
     , Readable (Auto f) (Image S (Y'CbCr SRGB) Word32)
     , Readable (Auto f) (Image S (Y'CbCr SRGB) Word64)
     -- (AdobeRGB 'NonLinear) doesn't have Luma instance
     -- , Readable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word8)
     -- , Readable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word16)
     -- , Readable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word32)
     -- , Readable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word64)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word8)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word16)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word32)
     , Readable (Auto f) (Image S (Alpha (Y D65)) Word64)
     , Readable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word8)
     , Readable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word16)
     , Readable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word32)
     , Readable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word64)
     , Readable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word8)
     , Readable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word16)
     , Readable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word32)
     , Readable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word64)
     , Readable (Auto f) (Image S (SRGB 'NonLinear) Word8)
     , Readable (Auto f) (Image S (SRGB 'NonLinear) Word16)
     , Readable (Auto f) (Image S (SRGB 'NonLinear) Word32)
     , Readable (Auto f) (Image S (SRGB 'NonLinear) Word64)
     , Readable (Auto f) (Image S (AdobeRGB 'NonLinear) Word8)
     , Readable (Auto f) (Image S (AdobeRGB 'NonLinear) Word16)
     , Readable (Auto f) (Image S (AdobeRGB 'NonLinear) Word32)
     , Readable (Auto f) (Image S (AdobeRGB 'NonLinear) Word64)
     , Writable (Auto f) (Image S (Y D65) Word8)
     , Writable (Auto f) (Image S (Y D65) Word16)
     , Writable (Auto f) (Image S (Y D65) Word32)
     , Writable (Auto f) (Image S (Y D65) Word64)
     , Writable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word8)
     , Writable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word16)
     , Writable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word32)
     , Writable (Auto f) (Image S (CMYK (SRGB 'NonLinear)) Word64)
     , Writable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word8)
     , Writable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word16)
     , Writable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word32)
     , Writable (Auto f) (Image S (CMYK (AdobeRGB 'NonLinear)) Word64)
     , Writable (Auto f) (Image S (Y'CbCr SRGB) Word8)
     , Writable (Auto f) (Image S (Y'CbCr SRGB) Word16)
     , Writable (Auto f) (Image S (Y'CbCr SRGB) Word32)
     , Writable (Auto f) (Image S (Y'CbCr SRGB) Word64)
     -- , Writable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word8)
     -- , Writable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word16)
     -- , Writable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word32)
     -- , Writable (Auto f) (Image S (YCbCr (AdobeRGB 'NonLinear)) Word64)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word8)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word16)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word32)
     , Writable (Auto f) (Image S (Alpha (Y D65)) Word64)
     , Writable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word8)
     , Writable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word16)
     , Writable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word32)
     , Writable (Auto f) (Image S (Alpha (SRGB 'NonLinear)) Word64)
     , Writable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word8)
     , Writable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word16)
     , Writable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word32)
     , Writable (Auto f) (Image S (Alpha (AdobeRGB 'NonLinear)) Word64)
     , Writable (Auto f) (Image S (SRGB 'NonLinear) Word8)
     , Writable (Auto f) (Image S (SRGB 'NonLinear) Word16)
     , Writable (Auto f) (Image S (SRGB 'NonLinear) Word32)
     , Writable (Auto f) (Image S (SRGB 'NonLinear) Word64)
     , Writable (Auto f) (Image S (AdobeRGB 'NonLinear) Word8)
     , Writable (Auto f) (Image S (AdobeRGB 'NonLinear) Word16)
     , Writable (Auto f) (Image S (AdobeRGB 'NonLinear) Word32)
     , Writable (Auto f) (Image S (AdobeRGB 'NonLinear) Word64)
     )
  => f
  -> Spec
specEncodeDecodeNoErrorAuto f =
    describe (show f) $ do
      specEncodeDecodeAutoNoError @(Y D65) @Word8 f
      specEncodeDecodeAutoNoError @(Y D65) @Word16 f
      specEncodeDecodeAutoNoError @(Y D65) @Word32 f
      specEncodeDecodeAutoNoError @(Y D65) @Word64 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word8 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word16 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word32 f
      specEncodeDecodeAutoNoError @(Alpha (Y D65)) @Word64 f
      specEncodeDecodeAutoNoError @(SRGB 'NonLinear) @Word8 f
      specEncodeDecodeAutoNoError @(SRGB 'NonLinear) @Word16 f
      specEncodeDecodeAutoNoError @(SRGB 'NonLinear) @Word32 f
      specEncodeDecodeAutoNoError @(SRGB 'NonLinear) @Word64 f
      specEncodeDecodeAutoNoError @(Alpha (SRGB 'NonLinear)) @Word8 f
      specEncodeDecodeAutoNoError @(Alpha (SRGB 'NonLinear)) @Word16 f
      specEncodeDecodeAutoNoError @(Alpha (SRGB 'NonLinear)) @Word32 f
      specEncodeDecodeAutoNoError @(Alpha (SRGB 'NonLinear)) @Word64 f
      specEncodeDecodeAutoNoError @(CMYK (SRGB 'NonLinear)) @Word8 f
      specEncodeDecodeAutoNoError @(CMYK (SRGB 'NonLinear)) @Word16 f
      specEncodeDecodeAutoNoError @(CMYK (SRGB 'NonLinear)) @Word32 f
      specEncodeDecodeAutoNoError @(CMYK (SRGB 'NonLinear)) @Word64 f
      specEncodeDecodeAutoNoError @(Y'CbCr SRGB) @Word8 f
      specEncodeDecodeAutoNoError @(Y'CbCr SRGB) @Word16 f
      specEncodeDecodeAutoNoError @(Y'CbCr SRGB) @Word32 f
      specEncodeDecodeAutoNoError @(Y'CbCr SRGB) @Word64 f
      specEncodeDecodeAutoNoError @(AdobeRGB 'NonLinear) @Word8 f
      specEncodeDecodeAutoNoError @(AdobeRGB 'NonLinear) @Word16 f
      specEncodeDecodeAutoNoError @(AdobeRGB 'NonLinear) @Word32 f
      specEncodeDecodeAutoNoError @(AdobeRGB 'NonLinear) @Word64 f
      specEncodeDecodeAutoNoError @(Alpha (AdobeRGB 'NonLinear)) @Word8 f
      specEncodeDecodeAutoNoError @(Alpha (AdobeRGB 'NonLinear)) @Word16 f
      specEncodeDecodeAutoNoError @(Alpha (AdobeRGB 'NonLinear)) @Word32 f
      specEncodeDecodeAutoNoError @(Alpha (AdobeRGB 'NonLinear)) @Word64 f
      specEncodeDecodeAutoNoError @(CMYK (AdobeRGB 'NonLinear)) @Word8 f
      specEncodeDecodeAutoNoError @(CMYK (AdobeRGB 'NonLinear)) @Word16 f
      specEncodeDecodeAutoNoError @(CMYK (AdobeRGB 'NonLinear)) @Word32 f
      specEncodeDecodeAutoNoError @(CMYK (AdobeRGB 'NonLinear)) @Word64 f
      -- specEncodeDecodeAutoNoError @(YCbCr (AdobeRGB 'NonLinear)) @Word8 f
      -- specEncodeDecodeAutoNoError @(YCbCr (AdobeRGB 'NonLinear)) @Word16 f
      -- specEncodeDecodeAutoNoError @(YCbCr (AdobeRGB 'NonLinear)) @Word32 f
      -- specEncodeDecodeAutoNoError @(YCbCr (AdobeRGB 'NonLinear)) @Word64 f
