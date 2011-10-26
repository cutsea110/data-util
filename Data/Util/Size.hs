{-# LANGUAGE MagicHash,UnboxedTuples #-}
module Data.Util.Size
       ( unsafeSizeof
       , strictUnsafeSizeof
       )where

import GHC.Exts
import Foreign

unsafeSizeof :: a -> Int
unsafeSizeof a =
  case unpackClosure# a of
    (# x, ptrs, nptrs #) ->
      sizeOf header + 
      I# (sizeofByteArray# (unsafeCoerce# ptrs) +# 
          sizeofByteArray# nptrs)
  where
    header :: Int
    header = undefined

strictUnsafeSizeof :: a -> Int
strictUnsafeSizeof = (unsafeSizeof $!)
