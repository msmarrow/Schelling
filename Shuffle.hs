{- File: Shuffle.hs
   Copyright: (C) Mahjeed Marrow 2019
   Description: Just an algorithm that randomly shuffles a list
   Runs in O(n)
-}

module Shuffle (
    shuffle
) where

import qualified System.Random
import Data.Array.IO
import Control.Monad

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
