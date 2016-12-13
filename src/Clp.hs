{-# LANGUAGE RecordWildCards #-}
module Clp
 ( Matrix (..)
 , Constraints (..)
 , defaultConstraints
 , LogLevel (..)
 , SolveType (..)
 , InfeasibleReturn (..)
 , Status (..)
 , Options (..)
 , defaultOptions
 , solve
 , solveWithOptions
 ) where


import Foreign.C.String       (CString, withCAString)
import Foreign.C.Types        (CDouble (..), CInt (..))
import Foreign.ForeignPtr     (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array  (peekArray)
import Foreign.Ptr            (FunPtr, Ptr, nullPtr)
import Foreign.Storable

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector.Storable   (Vector, fromList, unsafeWith)


--- * ffi ------------------------------------------------------------------------------------------------------------

type Model  = Ptr ()
type Solver = Ptr ()

foreign import ccall unsafe "Clp_newModel" newModel :: IO Model
foreign import ccall unsafe "&Clp_deleteModel" deleteModel :: FunPtr(Model -> IO ())
foreign import ccall unsafe "Clp_resize" resize :: Model -> CInt -> CInt -> IO ()
foreign import ccall unsafe "Clp_loadProblem" loadProblem
  :: Model
  -- sparse column major matrix representation
  -> CInt -- ^ nrows
  -> CInt -- ^ ncols
  -> Ptr CInt -- ^ 0:accumulated number of elements in colums
  -> Ptr CInt -- ^ (non-zero) indices in columns
  -- default behaviour whenn NULL
  -> Ptr CDouble -- ^ values
  -> Ptr CDouble -- ^ colub: all columns have upper bound infinity
  -> Ptr CDouble -- ^ collb: all columns have lower bound 0
  -> Ptr CDouble -- ^ rowlb: all rows have lower bound -infinity
  -> Ptr CDouble -- ^ rowub: all rows have upper bound infinity
  -> Ptr CDouble -- ^ objective function: all variables have 0 objective coefficient
  -> IO ()
foreign import ccall unsafe "ClpSolve_new" newSolver :: IO Solver
foreign import ccall unsafe "&ClpSolve_delete" deleteSolver :: FunPtr(Solver -> IO ())
foreign import ccall unsafe "Clp_initialSolve" initialSolve :: Model -> IO CInt
foreign import ccall unsafe "Clp_initialSolveWithOptions" initialSolveWithOptions :: Model -> Solver -> IO CInt
foreign import ccall unsafe "Clp_printModel" printModel :: Model -> CString -> IO ()
foreign import ccall unsafe "Clp_setLogLevel" setLogLevel :: Model -> CInt -> IO ()
foreign import ccall unsafe "ClpSolve_setSolveType" setSolveType :: Solver -> CInt -> CInt -> IO ()
foreign import ccall unsafe "ClpSolve_setInfeasibleReturn" setInfeasibleReturn :: Solver -> CInt -> IO ()
foreign import ccall unsafe "Clp_setOptimizationDirection" setDirection :: Model -> CDouble -> IO ()
foreign import ccall unsafe "Clp_setMaximumSeconds" setMaximumSeconds :: Model -> CDouble -> IO ()
foreign import ccall unsafe "Clp_primalColumnSolution" primalColumnsolution :: Model -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_getNumRows" getNumRows :: Model -> IO CInt

--- * model ----------------------------------------------------------------------------------------------------------

-- FIXME: MS:
-- representation of Infinity, NaN in Clp

-- TODO MS:
-- implement options

-- | Sparse Matrix in column-major format - Compressed Sparse Column (CSC)
-- column; value[start[i]] ~ value[start[i+1]-1]
data Matrix = Matrix
  { nRows :: CInt    -- ^ number of rows
  , nCols :: CInt    -- ^ number of columns
  , start :: Vector CInt  -- ^ start[0] = 0; start[i] = start[i-1] + number of nonzer elements in (i-1) column; @length start == nCols + 1@
  , index :: Vector CInt  -- ^ row index of each element; @length  index == length value@
  , value :: Vector CDouble -- ^ non-zero values in column-major format
  } deriving Show

data Constraints = Constraints
  { collb :: Maybe (Vector CDouble)
  , colub :: Maybe (Vector CDouble)
  , objtv :: Maybe (Vector CDouble)
  , rowlb :: Maybe (Vector CDouble)
  , rowub :: Maybe (Vector CDouble)
  } deriving Show

defaultConstraints :: Constraints
defaultConstraints = Constraints
  { collb = Nothing
  , colub = Nothing
  , objtv = Nothing
  , rowlb = Nothing
  , rowub = Nothing }


--- * solver options  ------------------------------------------------------------------------------------------------

data LogLevel = Zero | One | Two | Three | Four
  deriving (Show, Enum, Bounded)

data SolveType = DualSimplex | PrimalSimplex | Barrier | BarrierNoCrossover | Automatic
  deriving (Show, Enum, Bounded)

data InfeasibleReturn = InfeasibleReturn | NoInfeasibleReturn
  deriving (Show, Enum, Bounded)

data Status = Optimal | Infeasible | Unbounded | UserLimit | Error
  deriving (Show, Enum, Bounded)

toStatus :: CInt -> Status
toStatus = toEnum . fromIntegral

type Result = Vector Double

data Direction = Maximize | Ignore | Minimize
  deriving (Show, Enum, Bounded)

toDirection :: Direction -> CDouble
toDirection = CDouble . fromIntegral . (+ (-1)) . fromEnum

data Options = Options
  { logLevel         :: LogLevel
  , solveType        :: SolveType
  , infeasibleReturn :: InfeasibleReturn }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
  { logLevel         = Zero
  , solveType        = Automatic
  , infeasibleReturn = InfeasibleReturn }


unsafeWithM :: Storable a => Maybe (Vector a) -> (Ptr a -> IO b) -> IO b
unsafeWithM Nothing  f = f nullPtr
unsafeWithM (Just a) f = unsafeWith a f

solve :: MonadIO m => Matrix -> Constraints -> m (Maybe Result)
solve = solve' Nothing

solveWithOptions :: MonadIO m => Options -> Matrix -> Constraints -> m (Maybe Result)
solveWithOptions opt = solve' (Just opt)

withClp :: MonadIO m => Maybe Options -> Matrix -> Constraints -> (Maybe Options -> Model -> Solver -> IO b) -> m b
withClp optM Matrix{..} Constraints{..} f = liftIO $ do
  mFPtr <- newForeignPtr deleteModel =<< newModel
  withForeignPtr mFPtr $ \mPtr -> do
    resize mPtr nRows nCols
    unsafeWith start $ \startPtr ->
      unsafeWith index $ \indexPtr ->
        unsafeWith value $ \valuePtr ->
          unsafeWithM collb $ \collbPtr ->
            unsafeWithM colub $ \colubPtr ->
              unsafeWithM objtv $ \objtvPtr ->
                unsafeWithM rowlb $ \rowlbPtr ->
                  unsafeWithM rowub $ \rowubPtr -> do
                    loadProblem mPtr nRows nCols startPtr indexPtr valuePtr collbPtr colubPtr objtvPtr rowlbPtr rowubPtr
                    optFPtr <- newForeignPtr deleteSolver =<< newSolver
                    withForeignPtr optFPtr $ \optPtr ->
                      f optM mPtr optPtr

setOptions :: MonadIO m => Maybe Options -> Model -> Solver -> m ()
setOptions Nothing _ _                    = return ()
setOptions (Just Options{..}) mPtr optPtr = liftIO $ do
  setLogLevel mPtr (fromIntegral $ fromEnum logLevel)
  setSolveType optPtr (fromIntegral $ fromEnum solveType) (CInt 0)
  setInfeasibleReturn optPtr (fromIntegral $ fromEnum infeasibleReturn)

invokeSolver :: MonadIO m => Maybe Options -> Model -> Solver -> m Status
invokeSolver Nothing  mPtr _    = liftIO $ toStatus <$> initialSolve mPtr
invokeSolver (Just _) mPtr oPtr = liftIO $ toStatus <$> initialSolveWithOptions mPtr oPtr

getSolution :: MonadIO m => Status -> Model -> m (Maybe Result)
getSolution Optimal mPtr = liftIO $ do
  rPtr <- primalColumnsolution mPtr
  rs   <- fromIntegral <$> getNumRows mPtr
  Just . fromList . fmap fromCDouble <$> peekArray rs rPtr
  where fromCDouble (CDouble d) = d
getSolution _       mPtr = return Nothing

solve' :: MonadIO m => Maybe Options -> Matrix -> Constraints -> m (Maybe Result)
solve' optM m cs = withClp optM m cs $ \optM mPtr oPtr -> do
  setOptions optM mPtr oPtr
  status <- invokeSolver optM mPtr oPtr
  getSolution status mPtr

