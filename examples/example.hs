#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}

import Clp

import Foreign.C.Types      (CDouble (..))

import Data.Vector.Storable (fromList)

data DenseList = DenseList
  { dlNRows :: Int
  , dlNCols :: Int
  , dlValue :: [Double]
  , dlCollb :: Maybe [Double]
  , dlColub :: Maybe [Double]
  , dlObjtv :: Maybe [Double]
  , dlRowlb :: Maybe [Double]
  , dlRowub :: Maybe [Double]
  } deriving Show

dl2M :: DenseList -> Matrix
dl2M DenseList{..} = Matrix
  { nRows = fromIntegral dlNRows
  , nCols = fromIntegral dlNCols
  , start = fromList $ fmap fromIntegral $ take (dlNCols + 1) $ iterate (+dlNRows) 0
  , index = fromList $ fmap fromIntegral $ take (dlNCols * dlNRows)    $ cycle [0.. pred dlNRows]
  , value = fromList $ fmap CDouble dlValue
  }


main :: IO ()
main = do
  print =<< solveWithOptions
    defaultOptions{logLevel = Zero}
    (dl2M
      (DenseList
        3 3
        (take 9 [1.0,2.0..])
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        )
    )
    defaultConstraints
  print (0 :: Int)

