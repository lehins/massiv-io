{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.IO as A
import Data.ByteString as B
import Data.ByteString.Lazy as BL

encodeIO :: Writable f arr => f -> arr -> IO B.ByteString
encodeIO f img =
  BL.toStrict <$> encodeM f def img
{-# INLINE encodeIO #-}

benchDecode ::
     forall fr fw r cs e.
     ( Readable fr (Image S cs e)
     , Writable fw (Image S cs e)
     , Load r Ix2 (Pixel cs e)
     , ColorModel cs e
     )
  => String
  -> fr -- ^ Decode as
  -> fw -- ^ Encode as
  -> Image r cs e
  -> Benchmark
benchDecode name fr fw img =
  env (encodeIO fw (computeAs S img)) $ \bs ->
    bench name $ nfIO (decodeM fr bs :: IO (Image S cs e))


convertIO ::
     forall cs' e' i' r cs e i. (Source r (Pixel cs e), ColorSpace cs i e, ColorSpace cs' i' e')
  => Image r cs e
  -> IO (Image S cs' e')
convertIO = computeIO . convertImage
{-# INLINE convertIO #-}

main :: IO ()
main = do
  !frog <- readImageAuto "files/frog.jpg" :: IO (Image S (SRGB 'NonLinear) Double)
  defaultMain
    [ bgroup "Convert" [bench "Y'CbCr" $ nfIO (convertIO frog :: IO (Image S (Y'CbCr SRGB) Double))]
    , bgroup
        "Encode"
        [ bgroup
            "PNG"
            [ bgroup
                "Exact"
                [ env (convertIO frog) $ \(f :: Image S (SRGB 'NonLinear) Word8) ->
                    bench "SRGB Word8" $ nfIO (encodeIO PNG f)
                , env (convertIO frog) $ \(f :: Image S (SRGB 'NonLinear) Word16) ->
                    bench "SRGB Word16" $ nfIO (encodeIO PNG f)
                ]
            , bgroup
                "Auto"
                [ env (convertIO frog) $ \(f :: Image S (SRGB 'NonLinear) Word8) ->
                    bench "SRGB Word8" $ nfIO (encodeIO (Auto PNG) f)
                , env (convertIO frog) $ \(f :: Image S (SRGB 'NonLinear) Word16) ->
                    bench "SRGB Word16" $ nfIO (encodeIO (Auto PNG) f)
                , env (convertIO frog) $ \(f :: Image S (SRGB 'NonLinear) Word32) ->
                    bench "SRGB Word32" $ nfIO (encodeIO (Auto PNG) f)
                ]
            ]
        ]
    , bgroup
        "Decode"
        [ bgroup
            "PNG"
            [ bgroup
                "Exact"
                [ benchDecode "SRGB Word8" PNG PNG $
                  (convertImage frog :: Image D (SRGB 'NonLinear) Word8)
                , benchDecode "SRGB Word16" PNG PNG $
                  (convertImage frog :: Image D (SRGB 'NonLinear) Word16)
                ]
            , bgroup
                "Auto"
                [ benchDecode "SRGB Word8" (Auto PNG) PNG $
                  (convertImage frog :: Image D (SRGB 'NonLinear) Word8)
                , benchDecode "SRGB Word16" (Auto PNG) PNG $
                  (convertImage frog :: Image D (SRGB 'NonLinear) Word16)
                , env (encodeIO PNG =<< (convertIO frog :: IO (Image S (SRGB 'NonLinear) Word8))) $ \bs ->
                    bench "SRGB Word8 -> Word16" $
                    nfIO (decodeM (Auto PNG) bs :: IO (Image S (SRGB 'NonLinear) Word16))
                ]
            ]
        ]
    ]

