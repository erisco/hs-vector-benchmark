{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Main where



import Criterion.Main
import Control.Monad.Random
import Data.Functor.Identity
import Control.Applicative ((<$>))

import qualified Data.Vector.Generic as Vector
import Data.Vector.Generic (Vector)

import qualified Data.Vector.Generic.Mutable as MVector
import Data.Vector.Generic.Mutable (MVector)

import Data.Vector.Unboxed (Unbox)

import Control.Monad.ST
import Control.Monad.Primitive (PrimState, PrimMonad)
import Data.List (sort)


import Foreign.C.Types
import Foreign.Ptr

import qualified Data.Vector.Storable.Mutable as SMVector
import qualified Data.Vector.Storable as SVector

import qualified Data.Vector.Algorithms.Tim as Tim

foreign import ccall unsafe "insertion_sort_i" insertion_sort_i :: Ptr CInt -> CInt -> IO ()

foreign import ccall unsafe "introsort_i" introsort_i :: Ptr CInt -> CInt -> IO ()

randomVector :: (Vector v a, MonadRandom f, Random a, Functor f) =>
                Int -> a -> a -> f (v a)
randomVector n min max = Vector.fromList . take n <$> getRandomRs (min, max)


type family Immutable (v :: * -> * -> *) :: * -> *
type instance Immutable SMVector.MVector = SVector.Vector

randomMVector :: forall v v' a m. (Immutable v ~ v', Vector.Mutable v' ~ v, Vector v' a, PrimMonad m, MonadRandom m, Random a, Num a, Functor m) =>
                  Int -> a -> a -> m (v (PrimState m) a)
randomMVector n min max = (randomVector n min max :: m (v' a)) >>= Vector.thaw


revOrderVector :: (Enum a, Num a, Ord a, Monad m, Vector v a) => a -> m (v a)
revOrderVector n = return . Vector.fromList . reverse $ [1..n]

revOrderMVector :: forall v v' a m. (Immutable v ~ v', Vector.Mutable v' ~ v, Vector v' a, PrimMonad m, Ord a, Num a, Enum a) =>
     a -> m (v (PrimState m) a)
revOrderMVector n = (revOrderVector n :: m (v' a)) >>= Vector.thaw

{-
insertionSort :: forall v a. (Vector v a, Unbox a, Ord a) => v a -> v a
insertionSort immut_v = runST (mut_v >>= sort 0)

  where mut_v :: forall s. ST s (MVector (PrimState (ST s)) a)
        mut_v = Vector.thaw immut_v

        length_v = Vector.length immut_v

        sort i v
          | i < length_v = do vi <- MVector.unsafeRead v i
                              insert (i - 1) vi v
                              sort (i + 1) v
          | otherwise    = Vector.unsafeFreeze v

        insert i x v
          | i < 0     = MVector.unsafeWrite v 0 x
          | otherwise = do vi <- MVector.unsafeRead v i
                           if x < vi
                           then do MVector.unsafeWrite v (i + 1) vi
                                   insert (i - 1) x v
                           else MVector.unsafeWrite v (i + 1) x
--

-}
listSort :: (Vector v a, Ord a, Unbox a) => v a -> v a
listSort = Vector.fromList . sort . Vector.toList


-- env (randomVector 10 1 10)


main :: IO ()
main = defaultMain [
  bgroup "sort" [ --env (revOrderVector :: IO (Vector Int))
                  --env (randomVector 10 (1 :: Int) 10)--
                  --(\x -> bench "A" $ nf insertionSort x)
                --,
                  env (revOrderVector 100000 :: IO (SVector.Vector Int))
                  --env (randomVector 100000 1 10000 :: IO (SVector.Vector Int))
                  (\x -> bench "B" $ nf listSort x)
                ,--, bench "20" $ nfIO (fmap insertionSort)
               --, bench "30" $ nfIO (fmap insertionSort)
               --, bench "40" $ nfIO (fmap insertionSort)
                  --env (randomVector 100000 1 10000 :: IO (SVector.Vector CInt))
                  env (revOrderVector 100000 :: IO (SVector.Vector CInt))
                  --(\x -> bench "C" $ nfIO (flip SMVector.unsafeWith (flip insertion_sort_i 1000) x))
                  (\x -> bench "C" $ nfIO (flip SMVector.unsafeWith (flip introsort_i 100000) =<< Vector.thaw x))
                , --env (randomVector 100000 1 10000 :: IO (SVector.Vector CInt))
                  env (revOrderVector 100000 :: IO (SVector.Vector CInt))
                  (\x -> bench "C" $ nfIO (Tim.sort =<< Vector.thaw x))
               ]
  ]
--
