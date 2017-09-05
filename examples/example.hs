#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Clp
import qualified ClpM

import Data.Vector.Storable (fromList)
import qualified Data.List as L (transpose)


-- @
-- LP
--   2 2
--   [ [3, 4], [2, 3] ]
--   [ 650, 500]
--   [ 5, 7]
--
-- MAX 5x + 7y
-- 3x + 4y < 650
-- 4x + 3y < 500
-- @
data LP = LP
  { nRows :: Int
  , nCols :: Int
  , vals  :: [[Int]]
  , obnd  :: [Int]
  , ofun  :: [Int]
  } deriving Show


values :: Int -> [[Int]] -> [Int]
values n vs = concat $ L.transpose $ fmap k vs
  where k es = take n (es ++ cycle [0])

lp2M :: LP -> Clp.Matrix
lp2M LP{..} = Clp.Matrix
  { Clp.nRows = fromIntegral nRows
  , Clp.nCols = fromIntegral nCols
  , Clp.start = tod $ take (nCols + 1)     $ iterate (+nRows) 0
  , Clp.index = tod $ take (nCols * nRows) $ cycle [0.. pred nRows]
  , Clp.value = tod $ take (nCols * nRows) $ values nCols vals
  }

tod = fromList . fmap fromIntegral

lp2C :: LP -> Clp.Constraints
lp2C LP{..} = Clp.defaultConstraints
  { Clp.rowub = Clp.Some . tod $ take nRows $ obnd ++ cycle [0]
  , Clp.objtv = Clp.Some . tod $ take nCols $ ofun ++ cycle [0] }

maximize = Clp.setOptimizationDirection Clp.Maximize

lp =
  LP
    2 3
    [ [3, 4, 5], [2, 3, 4] ]
    [ 650, 500, 700 ]
    [ 5, 7, 8 ]

main :: IO ()
main = main1

main1 :: IO ()
main1 = do
  print =<< 
    Clp.solve' 
      [] 
      [Clp.setLogLevel Clp.Four, maximize]
      Clp.PrimalRowSolution
      (lp2M lp)
      (lp2C lp)
  print (0 :: Int)

main2 :: IO ()
main2 = do
  ClpM.withClp (lp2M lp) (lp2C lp) $ do
    ClpM.liftModel $ Clp.setOptimizationDirection Clp.Maximize
    ClpM.liftModel $ Clp.setOptimizationDirection Clp.Maximize
    ClpM.liftModel $ Clp.setLogLevel Clp.Four
    status <- ClpM.liftEnv Clp.initialSolveWithOptions
    return ()
    -- ClpM.printModel "YEAH"


