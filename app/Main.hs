{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.ByteString.Short
    ( ShortByteString
    , empty
    , fromShort
    , index
    , null
    , packCString
    , packCStringLen
    , toShort
    , useAsCString
    , useAsCStringLen
    )
import Data.ByteString.Short.Internal
    ( ShortByteString (SBS), createFromPtr )
import qualified Data.Foldable as F
import GHC.Exts
import GHC.ST
    ( ST (ST), runST )
import GHC.Word

import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as BS
import qualified Data.List as List

main :: IO ()
main = do
  let sbs = singleton _A
  putStrLn $ show sbs -- prints "\NULA"



writeWord16Array :: MBA s -> Int -> Word16 -> ST s ()
writeWord16Array (MBA# mba#) (I# i#) w@(W16# w#) =
  ST $ \s# ->
    case writeWord16Array# mba# i#
      w#
      s# of
        s# -> (# s#, () #)

singleton :: Word16 -> ShortByteString
singleton = \w -> create 2 (\mba -> writeWord16Array mba 0 w)

_A :: Word16
_A = 0x4100



data BA    = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)

create :: Int -> (forall s. MBA s -> ST s ()) -> ShortByteString
create len fill =
    runST $ do
      mba <- newByteArray len
      fill mba
      BA# ba# <- unsafeFreezeByteArray mba
      return (SBS ba#)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
    ST $ \s -> case newByteArray# len# s of
                 (# s, mba# #) -> (# s, MBA# mba# #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s, ba# #) -> (# s, BA# ba# #)
