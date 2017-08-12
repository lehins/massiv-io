{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Array.Massiv.Common.ShapeSpec (spec) where

import           Data.Array.Massiv
import           Data.Array.Massiv.Common.IndexSpec (Sz (..), SzIx (..))
import           Data.Array.Massiv.CommonSpec       (ArrIx (..))
import           Data.Array.Massiv.DelayedSpec      ()
import           Data.Array.Massiv.ManifestSpec     ()
import           Test.Hspec
import           Test.QuickCheck

-----------
-- Shape --
-----------

-- extract

prop_ExtractEqualsExtractFromTo
  :: (Eq (Array (R r) ix e), Arbitrary (Array r ix e), Shape r ix e)
  => proxy (r, ix, e) -> SzIx ix -> Array r ix e -> Bool
prop_ExtractEqualsExtractFromTo _ (SzIx (Sz eIx) sIx) arr =
  extractFromTo sIx eIx arr == extract sIx (liftIndex2 (-) eIx sIx) arr




specShapeN
  :: (Eq (Array (R r) ix e), Arbitrary (Array r ix e), Show (Array r ix e), Arbitrary ix, Shape r ix e)
  => proxy (r, ix, e) -> Spec
specShapeN proxy = do
  describe "extract" $ do
    it "ExtractEqualsExtractFromTo" $ property $ prop_ExtractEqualsExtractFromTo proxy


-----------
-- Slice --
-----------


prop_SliceRight :: (Eq (Array (R r) (Lower ix) e), Arbitrary (Array r ix e), Slice r ix e)
  => proxy (r, ix, e) -> Int -> Array r ix e -> Bool
prop_SliceRight _ i arr = arr !?> i == arr <!?> (1, i)


prop_SliceLeft :: (Eq (Array (R r) (Lower ix) e), Arbitrary (Array r ix e), Slice r ix e)
  => proxy (r, ix, e) -> Int -> Array r ix e -> Bool
prop_SliceLeft _ i arr = arr <!? i == arr <!?> (rank (size arr), i)


prop_SliceIndexDim2D :: ArrIx D Ix2 Int -> Bool
prop_SliceIndexDim2D (ArrIx arr ix@(i :. j)) =
  val == evaluateAt (arr <! j) i &&
  val == evaluateAt (arr !> i) j
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim2RankD :: ArrIx D Ix2 Int -> Bool
prop_SliceIndexDim2RankD (ArrIx arr ix@(i :. j)) =
  val == evaluateAt (arr <!> (1, i)) j &&
  val == evaluateAt (arr <!> (2, j)) i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim3D :: ArrIx D Ix3 Int -> Bool
prop_SliceIndexDim3D (ArrIx arr ix@(i :> j :. k)) =
  val == evaluateAt (arr <! k <! j) i &&
  val == evaluateAt (arr !> i !> j) k &&
  val == evaluateAt (arr <! k !> i) j &&
  val == evaluateAt (arr !> i <! k) j
  where
    val = unsafeIndex arr ix

prop_SliceIndexDim3RankD :: ArrIx D Ix3 Int -> Bool
prop_SliceIndexDim3RankD (ArrIx arr ix@(i :> j :. k)) =
  val == evaluateAt (arr <!> (1, i) <!> (1, j)) k &&
  val == evaluateAt (arr <!> (1, i) <!> (2, k)) j &&
  val == evaluateAt (arr <!> (2, j) <!> (1, i)) k &&
  val == evaluateAt (arr <!> (2, j) <!> (2, k)) i &&
  val == evaluateAt (arr <!> (3, k) <!> (1, i)) j &&
  val == evaluateAt (arr <!> (3, k) <!> (2, j)) i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim2M :: ArrIx M Ix2 Int -> Bool
prop_SliceIndexDim2M (ArrIx arr ix@(i :. j)) =
  val == arr <!> (1, i) ! j &&
  val == arr <!> (2, j) ! i
  where
    val = unsafeIndex arr ix

prop_SliceIndexDim2RankM :: ArrIx M Ix2 Int -> Bool
prop_SliceIndexDim2RankM (ArrIx arr ix@(i :. j)) =
  val == arr <!> (1, i) ! j &&
  val == arr <!> (2, j) ! i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim3M :: ArrIx M Ix3 Int -> Bool
prop_SliceIndexDim3M (ArrIx arr ix@(i :> j :. k)) =
  val == arr <! k <! j ! i &&
  val == arr !> i !> j ! k &&
  val == arr <! k !> i ! j &&
  val == arr !> i <! k ! j
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim3RankM :: ArrIx M Ix3 Int -> Bool
prop_SliceIndexDim3RankM (ArrIx arr ix@(i :> j :. k)) =
  val == arr <!> (1, i) <!> (1, j) ! k &&
  val == arr <!> (1, i) <!> (2, k) ! j &&
  val == arr <!> (2, j) <!> (1, i) ! k &&
  val == arr <!> (2, j) <!> (2, k) ! i &&
  val == arr <!> (3, k) <!> (1, i) ! j &&
  val == arr <!> (3, k) <!> (2, j) ! i
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim4D :: ArrIx D Ix4 Int -> Bool
prop_SliceIndexDim4D (ArrIx arr ix@(i1 :> i2 :> i3 :. i4)) =
  val == evaluateAt (arr !> i1 !> i2 !> i3) i4 &&
  val == evaluateAt (arr !> i1 !> i2 <! i4) i3 &&
  val == evaluateAt (arr !> i1 <! i4 <! i3) i2 &&
  val == evaluateAt (arr !> i1 <! i4 !> i2) i3 &&
  val == evaluateAt (arr <! i4 !> i1 !> i2) i3 &&
  val == evaluateAt (arr <! i4 !> i1 <! i3) i2 &&
  val == evaluateAt (arr <! i4 <! i3 <! i2) i1 &&
  val == evaluateAt (arr <! i4 <! i3 !> i1) i2
  where
    val = unsafeIndex arr ix

prop_SliceIndexDim4RankD :: ArrIx D Ix4 Int -> Bool
prop_SliceIndexDim4RankD (ArrIx arr ix@(i1 :> i2 :> i3 :. i4)) =
  val == unsafeIndex (arr <!> (1, i1) <!> (1, i2) <!> (1, i3)) i4 &&
  val == unsafeIndex (arr <!> (1, i1) <!> (2, i3) <! i4) i2 &&
  val == unsafeIndex (arr <!> (2, i2) <!> (1, i1)) (i3 :. i4) &&
  val == unsafeIndex (arr <!> (3, i3) <!> (2, i2)) (i1 :. i4) &&
  val == unsafeIndex (arr <!> (3, i3) <!> (3, i4) !> i1) i2 &&
  val == unsafeIndex (arr <!> (4, i4) !> i1 !> i2) i3
  where
    val = evaluateAt arr ix

prop_SliceIndexDim4M :: ArrIx M Ix4 Int -> Bool
prop_SliceIndexDim4M (ArrIx arr ix@(i1 :> i2 :> i3 :. i4)) =
  val == arr !> i1 !> i2 !> i3 ! i4 &&
  val == arr !> i1 !> i2 <! i4 ! i3 &&
  val == arr !> i1 <! i4 <! i3 ! i2 &&
  val == arr !> i1 <! i4 !> i2 ! i3 &&
  val == arr <! i4 !> i1 !> i2 ! i3 &&
  val == arr <! i4 !> i1 <! i3 ! i2 &&
  val == arr <! i4 <! i3 <! i2 ! i1 &&
  val == arr <! i4 <! i3 !> i1 ! i2
  where
    val = unsafeIndex arr ix


prop_SliceIndexDim4RankM :: ArrIx M Ix4 Int -> Bool
prop_SliceIndexDim4RankM (ArrIx arr ix@(i1 :> i2 :> i3 :. i4)) =
  val == arr <!> (1, i1) <!> (1, i2) <!> (1, i3) ! i4 &&
  val == arr <!> (1, i1) <!> (2, i3) <! i4 ! i2 &&
  val == arr <!> (2, i2) <!> (1, i1) ! (i3 :. i4) &&
  val == arr <!> (3, i3) <!> (2, i2) ! (i1 :. i4) &&
  val == arr <!> (3, i3) <!> (3, i4) !> i1 ! i2 &&
  val == arr <!> (4, i4) !> i1 !> i2 ! i3
  where
    val = unsafeIndex arr ix


specSliceN :: ( Eq (Array (R r) (Lower ix) e)
              , Arbitrary (Array r ix e)
              , Show (Array r ix e)
              , Arbitrary ix
              , Slice r ix e
              )
           => proxy (r, ix, e) -> Spec
specSliceN proxy = do
  describe "Slice" $ do
    it "SliceRight" $ property $ prop_SliceRight proxy
    it "SliceLeft" $ property $ prop_SliceLeft proxy



spec :: Spec
spec = do
  describe "Ix1" $ do
    specShapeN (Nothing :: Maybe (D, Ix1, Int))
  describe "Ix2" $ do
    specShapeN (Nothing :: Maybe (D, Ix2, Int))
    specSliceN (Nothing :: Maybe (D, Ix2, Int))
    describe "SliceIndex" $ do
      it "Delayed" $ property $ prop_SliceIndexDim2D
      it "Rank - Delayed" $ property $ prop_SliceIndexDim2RankD
      it "Manifest" $ property $ prop_SliceIndexDim2M
      it "Rank - Manifest" $ property $ prop_SliceIndexDim2RankM
  describe "Ix3" $ do
    specShapeN (Nothing :: Maybe (D, Ix3, Int))
    specSliceN (Nothing :: Maybe (D, Ix3, Int))
    describe "SliceIndex" $ do
      it "Delayed" $ property $ prop_SliceIndexDim3D
      it "Rank - Delayed" $ property $ prop_SliceIndexDim3RankD
      it "Manifest" $ property $ prop_SliceIndexDim3M
      it "Rank - Manifest" $ property $ prop_SliceIndexDim3RankM
  describe "Ix4" $ do
    specShapeN (Nothing :: Maybe (D, Ix4, Int))
    specSliceN (Nothing :: Maybe (D, Ix4, Int))
    describe "SliceIndex" $ do
      it "Delayed" $ property $ prop_SliceIndexDim4D
      it "Rank - Delayed" $ property $ prop_SliceIndexDim4RankD
      it "Manifest" $ property $ prop_SliceIndexDim4M
      it "Rank - Manifest" $ property $ prop_SliceIndexDim4RankM
