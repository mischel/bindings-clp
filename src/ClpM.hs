{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ClpM where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)

import           Clp (Solver, Model, Matrix, Constraints)
import qualified Clp as Clp


class MonadIO m => MonadClp m where
  getSolver :: m Solver
  getModel  :: m Model


data Env = Env { solver :: Solver , model  :: Model }

newtype ClpM a = ClpM { runClpM :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance MonadClp ClpM where
  getSolver = ClpM $ asks solver
  getModel  = ClpM $ asks model

withClp :: Matrix -> Constraints -> ClpM a -> IO a
withClp m cs clpM = Clp.withClp m cs $ \mPtr oPtr ->
  runReaderT
   (runClpM clpM)
   Env{solver = oPtr, model = mPtr}

liftModel :: (Model -> IO a) -> ClpM a
liftModel f = getModel >>= liftIO . f

liftSolver :: (Solver -> IO a) -> ClpM a
liftSolver f = getSolver >>= liftIO . f

liftEnv :: (Model -> Solver -> IO a) -> ClpM a
liftEnv f = getSolver >>= \s -> getModel >>= \m -> liftIO $ f m s

printModel :: String -> ClpM ()
printModel = liftModel . Clp.printModel

