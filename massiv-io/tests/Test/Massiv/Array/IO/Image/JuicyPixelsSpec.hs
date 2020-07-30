{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Massiv.Array.IO.Image.JuicyPixelsSpec (spec) where

import Data.Massiv.Array.IO
import Test.Massiv.Core
import Test.Massiv.Array.IO.Image.Common

spec :: Spec
spec =
  describe "Readable/Writable" $ do
    describe "BMP" $ do
      specEncodeNoError @(Y' SRGB) @Word8 BMP
      specEncodeNoError @(Y D65) @Word8 BMP
      --specEncodeDecodeNoError @(Y D65) @Word8 BMP
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word8 BMP
      specEncodeDecodeNoError @(Alpha (SRGB 'NonLinear)) @Word8 BMP
    describe "GIF" $ do
      specEncodeNoError @(Y' SRGB) @Word8 GIF
      specEncodeNoError @(Y D65) @Word8 GIF
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word8 GIF
      --specEncodeGifSequenceNoError @(Y D65) @Word8
      describe "Sequenece" $ do
        specEncodeDecodeGifSequenceNoError @(SRGB 'NonLinear) @Word8
        specEncodeDecodeGifSequenceNoError @(Alpha (SRGB 'NonLinear)) @Word8
      -- TODO: read RGBA8, write Y8
    -- getting 'DecodeError "Invalid sanline size"' with seed=2023820902
    -- describe "HDR" $ do
    --   specEncodeDecodeNoError @(SRGB 'NonLinear) @Float HDR
    describe "JPG" $ do
      specEncodeDecodeNoError @(Y' SRGB) @Word8 JPG
      specEncodeDecodeNoError @(Y D65) @Word8 JPG
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word8 JPG
      specEncodeDecodeNoError @(CMYK (SRGB 'NonLinear)) @Word8 JPG
      specEncodeDecodeNoError @(Y'CbCr SRGB) @Word8 JPG
      -- TODO: read YA8
    describe "PNG" $ do
      specEncodeDecodeNoError @(Y' SRGB) @Word8 PNG
      specEncodeDecodeNoError @(Y' SRGB) @Word16 PNG
      specEncodeDecodeNoError @(Alpha (Y' SRGB)) @Word8 PNG
      specEncodeDecodeNoError @(Alpha (Y' SRGB)) @Word16 PNG
      specEncodeDecodeNoError @(Y D65) @Word8 PNG
      specEncodeDecodeNoError @(Y D65) @Word16 PNG
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word8 PNG
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word16 PNG
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word8 PNG
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word16 PNG
      specEncodeDecodeNoError @(Alpha (SRGB 'NonLinear)) @Word8 PNG
      specEncodeDecodeNoError @(Alpha (SRGB 'NonLinear)) @Word16 PNG
    describe "TGA" $ do
      specEncodeDecodeNoError @(Y' SRGB) @Word8 TGA
      specEncodeDecodeNoError @(Y D65) @Word8 TGA
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word8 TGA
      specEncodeDecodeNoError @(Alpha (SRGB 'NonLinear)) @Word8 TGA
    describe "TIF" $ do
      specEncodeDecodeNoError @(Y' SRGB) @Word8 TIF
      specEncodeDecodeNoError @(Y' SRGB) @Word16 TIF
      specEncodeDecodeNoError @(Y' SRGB) @Word32 TIF
      specEncodeDecodeNoError @(Y' SRGB) @Float TIF
      specEncodeDecodeNoError @(Y D65) @Word8 TIF
      specEncodeDecodeNoError @(Y D65) @Word16 TIF
      specEncodeDecodeNoError @(Y D65) @Word32 TIF
      specEncodeDecodeNoError @(Y D65) @Float TIF
      specEncodeDecodeNoError @(Alpha (Y' SRGB)) @Word8 TIF
      specEncodeDecodeNoError @(Alpha (Y' SRGB)) @Word16 TIF
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word8 TIF
      specEncodeDecodeNoError @(Alpha (Y D65)) @Word16 TIF
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word8 TIF
      specEncodeDecodeNoError @(SRGB 'NonLinear) @Word16 TIF
      specEncodeDecodeNoError @(Alpha (SRGB 'NonLinear)) @Word8 TIF
      specEncodeDecodeNoError @(Alpha (SRGB 'NonLinear)) @Word16 TIF
      specEncodeDecodeNoError @(CMYK (SRGB 'NonLinear)) @Word8 TIF
      specEncodeDecodeNoError @(CMYK (SRGB 'NonLinear)) @Word16 TIF
      specEncodeNoError @(Y'CbCr SRGB) @Word8 TIF
