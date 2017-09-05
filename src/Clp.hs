{-# LANGUAGE RecordWildCards #-}
-- | This module provides a wrapper for the basic functionality of the /Clp/ solver.
module Clp
 (
 -- * Problem
 Matrix (..)
 , VectorM (..)
 , Constraints (..)
 , defaultConstraints
 -- * Configuration
 -- ** Solver Options
 , Solver
 , SolveType (..)
 , PresolveType (..)
 , InfeasibleReturn (..)
 , setSolveType
 , setPresolveType
 , setInfeasibleReturn
 , fromOption
 -- ** Model Configuration
 , Model
 , printModel
 , setMaximumIterations
 , setMaximumSeconds
 , LogLevel (..)
 , setLogLevel
 , Direction (..)
 , setOptimizationDirection
 -- * Action
 , withClp
 -- * Solve
 , Status (..)
 , solve
 , solve'
 , initialSolve
 , initialSolveWithOptions
 -- * Solution
 , Solution
 , Goal (..)
 , getSolution
 , getSolution'
 ) where


import Foreign.C.Types        (CDouble (..), CInt (..))
import Foreign.C.String       (withCString)
import Foreign.ForeignPtr     (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array  (peekArray)
import Foreign.Ptr            (nullPtr)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector.Storable   (Vector, fromList, unsafeWith)

import qualified Bindings.ClpCInterface as B


--- * Problem --------------------------------------------------------------------------------------------------------

type Solver = B.ClpSolve

-- | VectorM a ~ Maybe (Vector a)
data VectorM a = Some (Vector a) | Null deriving Show


-- | Sparse Matrix in Compressed Sparse Column (CSC) representation.
--
-- Following @3x4@ Matrix
--
-- @
-- [ 1 2 0 0 ]
-- [ 0 0 7 0 ]
-- [ 9 0 1 2 ]
--
-- is represented as follows:
--
-- Matrix {
--   , nRows = 3
--   , nCols = 4
--   , start = [ 0 2 3 5 6 ]
--   , index = [ 0 2 0 1 2 2 ]
--   , value = [ 1 9 2 7 1 2 ]
-- }
-- @
data Matrix = Matrix
  { nRows :: CInt           -- ^ number of rows
  , nCols :: CInt           -- ^ number of columns
  , start :: Vector CInt    -- ^ start indices
                            --
                            -- > length start == nCols + 1
                            -- > start[0] == 0
                            -- > start[i] == start[i-1] + number of non-zero column entries in i-th column
  , index :: Vector CInt    -- ^ row indices of non-zero elements (0-based indexing)
                            --
                            -- > length index == length value
  , value :: Vector CDouble -- ^ non zero element vector in column-major format
  } deriving Show

-- | Constraints
--
-- Default constraints for @Null@.
--
--   * all columns have upper bound infinity
--   * all columns have lower bound 0
--   * all rows have upper bound infinity
--   * all rows have lower bound -infinity
--   * all variables have 0 objective coefficient
data Constraints = Constraints
  { collb :: VectorM CDouble
  , colub :: VectorM CDouble
  , objtv :: VectorM CDouble
  , rowlb :: VectorM CDouble
  , rowub :: VectorM CDouble
  } deriving Show

defaultConstraints :: Constraints
defaultConstraints = Constraints
  { collb = Null
  , colub = Null
  , objtv = Null
  , rowlb = Null
  , rowub = Null }


--- * Solver Options  ------------------------------------------------------------------------------------------------

data SolveType = DualSimplex | PrimalSimplex | PrimalOrSprint | Barrier | BarrierNoCrossover | Automatic
  deriving (Show, Enum, Bounded)

data PresolveType = On | Off | Number | NumberCost
  deriving (Show, Enum, Bounded)

data InfeasibleReturn = InfeasibleReturn | NoInfeasibleReturn
  deriving (Show, Enum, Bounded)

fromOption :: Enum a => a -> CInt
fromOption = fromIntegral . fromEnum

setSolveType :: SolveType -> Solver -> IO ()
setSolveType t oPtr = B.setSolveType oPtr (fromOption t) (CInt (-1))

setPresolveType :: PresolveType -> Solver -> IO ()
setPresolveType t oPtr = B.setPresolveType oPtr (fromOption t) (CInt (-1))

setInfeasibleReturn :: InfeasibleReturn -> Solver -> IO ()
setInfeasibleReturn s oPtr = B.setInfeasibleReturn oPtr (fromOption s)


--- * Model Configuration --------------------------------------------------------------------------------------------

type Model  = B.ClpSimplex

printModel :: String -> Model -> IO ()
printModel s m = withCString s (B.printModel m)

data LogLevel = One | Two | Three | Four
  deriving (Show, Enum, Bounded)

setLogLevel :: LogLevel -> Model -> IO ()
setLogLevel l = flip B.setLogLevel (fromOption l)

setMaximumIterations :: Int -> Model -> IO ()
setMaximumIterations n = flip B.setMaximumIterations (fromIntegral $ max 0 n)

setMaximumSeconds :: Int -> Model -> IO ()
setMaximumSeconds n = flip B.setMaximumSeconds (fromIntegral $ max 0 n)

data Direction = Maximize | Ignore | Minimize
  deriving (Show, Enum, Bounded)

toDirection :: Direction -> CDouble
toDirection = fromIntegral . pred . fromEnum

setOptimizationDirection :: Direction -> Model -> IO ()
setOptimizationDirection d = flip B.setOptimizationDirection (toDirection d)

data Status = Optimal | Infeasible | Unbounded | UserLimit | Error
  deriving (Show, Enum, Bounded)

toStatus :: CInt -> Status
toStatus = toEnum . fromIntegral


type Solution = Either Status (Vector Double)


solve :: MonadIO m => Matrix -> Constraints -> m Solution
solve m cs = withClp m cs $ \mPtr _ -> do
  status <- liftIO $ toStatus <$> B.initialSolve mPtr
  getSolution status mPtr

solve' :: MonadIO m => [Solver -> IO()] -> [Model -> IO ()] -> Goal -> Matrix -> Constraints -> m Solution
solve' opts cfgs goal m cs = withClp m cs $ \mPtr oPtr -> do
  mapM_ ($ oPtr) opts
  mapM_ ($ mPtr) cfgs
  status <- initialSolveWithOptions mPtr oPtr
  getSolution' goal status mPtr

withClp :: MonadIO m => Matrix -> Constraints -> (Model -> Solver -> IO b) -> m b
withClp Matrix{..} Constraints{..} f = liftIO $ do
  mFPtr <- newForeignPtr B.deleteModelF =<< B.newModel
  withForeignPtr mFPtr $ \mPtr -> do
    B.resize mPtr nRows nCols
    unsafeWith start $ \startPtr ->
      unsafeWith index $ \indexPtr ->
        unsafeWith value $ \valuePtr ->
          unsafeVectorM collb $ \collbPtr ->
            unsafeVectorM colub $ \colubPtr ->
              unsafeVectorM objtv $ \objtvPtr ->
                unsafeVectorM rowlb $ \rowlbPtr ->
                  unsafeVectorM rowub $ \rowubPtr -> do
                    B.loadProblem mPtr nCols nRows startPtr indexPtr valuePtr collbPtr colubPtr objtvPtr rowlbPtr rowubPtr
                    optFPtr <- newForeignPtr B.deleteSolverF =<< B.newSolver
                    withForeignPtr optFPtr $ \optPtr ->
                      f mPtr optPtr
  where
    unsafeVectorM (Some v) g = unsafeWith v g
    unsafeVectorM Null g     = g nullPtr

data Goal = PrimalRowSolution | PrimalColumnSolution | DualRowSolution | DualColumnSolution

initialSolve :: MonadIO m => Model -> m Status
initialSolve mPtr = liftIO $ toStatus <$> B.initialSolve mPtr

initialSolveWithOptions :: MonadIO m => Model -> Solver -> m Status
initialSolveWithOptions mPtr oPtr = liftIO $ toStatus <$> B.initialSolveWithOptions mPtr oPtr

getSolution :: MonadIO m => Status -> Model -> m Solution
getSolution = getSolution' PrimalRowSolution

getSolution' :: MonadIO m => Goal -> Status -> Model -> m Solution
getSolution' goal Optimal mPtr = liftIO $ do
  let (kind,count) = case goal of
        PrimalRowSolution     -> (B.primalColumnSolution, B.getNumRows)
        PrimalColumnSolution  -> (B.primalColumnSolution, B.getNumCols)
        DualRowSolution       -> (B.dualColumnSolution, B.getNumRows)
        DualColumnSolution    -> (B.dualColumnSolution, B.getNumCols)
  rPtr <- kind mPtr
  rs   <- fromIntegral <$> count mPtr
  Right . fromList . fmap fromCDouble <$> peekArray rs rPtr
  where fromCDouble (CDouble d) = d
getSolution' _ st _ = return (Left st)

