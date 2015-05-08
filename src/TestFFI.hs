{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types
import Foreign.Ptr

import Data.Vector.Storable.Mutable as MVector
import Data.Vector.Storable as Vector

foreign import ccall unsafe "insertion_sort_i" insertion_sort_i :: Ptr CInt -> CInt -> IO ()

main :: IO ()
main = do
  v <- Vector.thaw (Vector.fromList [5,3,7,1,3,0])
  MVector.unsafeWith v (flip insertion_sort_i (fromIntegral (MVector.length v) :: CInt))
  v' <- Vector.freeze v
  putStrLn (show . Vector.toList $ v')
--
