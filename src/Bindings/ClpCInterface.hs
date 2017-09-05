-- | This module provides Haskell bindings for the /CLP/<https://www.coin-or.org/Clp> linear programming solver.
--
-- CLP release version 1.16.10
-- @src/Clp/Clp_C_Interface.h@
--
-- Interface generated using /c2hsc/<https://hackage.haskell.org/package/c2hsc>.
module Bindings.ClpCInterface where


import Foreign.C.String (CString)
import Foreign.C.Types  (CDouble (CDouble), CInt (CInt), CUChar)
import Foreign.Ptr      (FunPtr, Ptr)


type ClpSimplex = Ptr ()
type ClpSolve   = Ptr ()

-- only true if compiled with @COIN_BIG_INDEX == 0@ (default)
type CoinBigIndex = CInt


--- * Clp Simplex ----------------------------------------------------------------------------------------------------

foreign import ccall "Clp_Version"                       version                       :: IO CString
foreign import ccall "Clp_VersionMajor"                  versionMajor                  :: IO CInt
foreign import ccall "Clp_VersionMinor"                  versionMinor                  :: IO CInt
foreign import ccall "Clp_VersionRelease"                versionRelease                :: IO CInt

foreign import ccall "Clp_newModel"                      newModel                      :: IO ClpSimplex
foreign import ccall "Clp_deleteModel"                   deleteModel                   :: ClpSimplex -> IO ()
foreign import ccall "Clp_loadProblem"                   loadProblem                   :: ClpSimplex -> CoinBigIndex -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall "Clp_loadQuadraticObjective"        loadQuadraticObjective        :: ClpSimplex -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
foreign import ccall "Clp_readMps"                       readMps                       :: ClpSimplex -> CString -> CInt -> CInt -> IO CInt
foreign import ccall "Clp_copyInIntegerInformation"      copyInIntegerInformation      :: ClpSimplex -> CString -> IO ()
foreign import ccall "Clp_deleteIntegerInformation"      deleteIntegerInformation      :: ClpSimplex -> IO ()
foreign import ccall "Clp_resize"                        resize                        :: ClpSimplex -> CInt -> CInt -> IO ()
foreign import ccall "Clp_deleteRows"                    deleteRows                    :: ClpSimplex -> CInt -> Ptr CInt -> IO ()
foreign import ccall "Clp_addRows"                       addRows                       :: ClpSimplex -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
foreign import ccall "Clp_deleteColumns"                 deleteColumns                 :: ClpSimplex -> CInt -> Ptr CInt -> IO ()
foreign import ccall "Clp_addColumns"                    addColumns                    :: ClpSimplex -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
foreign import ccall "Clp_chgRowLower"                   chgRowLower                   :: ClpSimplex -> Ptr CDouble -> IO ()
foreign import ccall "Clp_chgRowUpper"                   chgRowUpper                   :: ClpSimplex -> Ptr CDouble -> IO ()
foreign import ccall "Clp_chgColumnLower"                chgColumnLower                :: ClpSimplex -> Ptr CDouble -> IO ()
foreign import ccall "Clp_chgColumnUpper"                chgColumnUpper                :: ClpSimplex -> Ptr CDouble -> IO ()
foreign import ccall "Clp_chgObjCoefficients"            chgObjCoefficients            :: ClpSimplex -> Ptr CDouble -> IO ()
foreign import ccall "Clp_dropNames"                     dropNames                     :: ClpSimplex -> IO ()
foreign import ccall "Clp_copyNames"                     copyNames                     :: ClpSimplex -> Ptr CString -> Ptr CString -> IO ()
foreign import ccall "Clp_numberRows"                    numberRows                    :: ClpSimplex -> IO CInt
foreign import ccall "Clp_numberColumns"                 numberColumns                 :: ClpSimplex -> IO CInt
foreign import ccall "Clp_primalTolerance"               primalTolerance               :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setPrimalTolerance"            setPrimalTolerance            :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_dualTolerance"                 dualTolerance                 :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setDualTolerance"              setDualTolerance              :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_dualObjectiveLimit"            dualObjectiveLimit            :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setDualObjectiveLimit"         setDualObjectiveLimit         :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_objectiveOffset"               objectiveOffset               :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setObjectiveOffset"            setObjectiveOffset            :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_problemName"                   problemName                   :: ClpSimplex -> CInt -> CString -> IO ()
foreign import ccall "Clp_setProblemName"                setProblemName                :: ClpSimplex -> CInt -> CString -> IO CInt
foreign import ccall "Clp_numberIterations"              numberIterations              :: ClpSimplex -> IO CInt
foreign import ccall "Clp_setNumberIterations"           setNumberIterations           :: ClpSimplex -> CInt -> IO ()
foreign import ccall "maximumIterations"                 maximumiterations             :: ClpSimplex -> IO CInt
foreign import ccall "Clp_setMaximumIterations"          setMaximumIterations          :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_maximumSeconds"                maximumSeconds                :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setMaximumSeconds"             setMaximumSeconds             :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_hitMaximumIterations"          hitMaximumIterations          :: ClpSimplex -> IO CInt
foreign import ccall "Clp_status"                        status                        :: ClpSimplex -> IO CInt
foreign import ccall "Clp_setProblemStatus"              setProblemStatus              :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_secondaryStatus"               secondaryStatus               :: ClpSimplex -> IO CInt
foreign import ccall "Clp_setSecondaryStatus"            setSecondaryStatus            :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_optimizationDirection"         optimizationDirection         :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setOptimizationDirection"      setOptimizationDirection      :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_primalRowSolution"             primalRowSolution             :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_primalColumnSolution"          primalColumnSolution          :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_dualRowSolution"               dualRowSolution               :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_dualColumnSolution"            dualColumnSolution            :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_rowLower"                      rowLower                      :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_rowUpper"                      rowUpper                      :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_objective"                     objective                     :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_columnLower"                   columnLower                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_columnUpper"                   columnUpper                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getNumElements"                getNumElements                :: ClpSimplex -> IO CInt
foreign import ccall "Clp_getVectorStarts"               getVectorStarts               :: ClpSimplex -> IO (Ptr CInt)
foreign import ccall "Clp_getIndices"                    getIndices                    :: ClpSimplex -> IO (Ptr CInt)
foreign import ccall "Clp_getVectorLengths"              getVectorLengths              :: ClpSimplex -> IO (Ptr CInt)
foreign import ccall "Clp_getElements"                   getElements                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_objectiveValue"                objectiveValue                :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_integerInformation"            integerInformation            :: ClpSimplex -> IO CString
foreign import ccall "Clp_infeasibilityRay"              infeasibilityRay              :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_unboundedRay"                  unboundedRay                  :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_freeRay"                       freeRay                       :: ClpSimplex -> Ptr CDouble -> IO ()
foreign import ccall "Clp_statusExists"                  statusExists                  :: ClpSimplex -> IO CInt
foreign import ccall "Clp_statusArray"                   statusArray                   :: ClpSimplex -> IO (Ptr CUChar)
foreign import ccall "Clp_copyinStatus"                  copyinStatus                  :: ClpSimplex -> Ptr CUChar -> IO ()
foreign import ccall "Clp_getColumnStatus"               getColumnStatus               :: ClpSimplex -> CInt -> IO CInt
foreign import ccall "Clp_getRowStatus"                  getRowStatus                  :: ClpSimplex -> CInt -> IO CInt
foreign import ccall "Clp_setColumnStatus"               setColumnStatus               :: ClpSimplex -> CInt -> CInt -> IO ()
foreign import ccall "Clp_setRowStatus"                  setRowStatus                  :: ClpSimplex -> CInt -> CInt -> IO ()
foreign import ccall "Clp_setUserPointer"                setUserPointer                :: ClpSimplex -> Ptr () -> IO ()
foreign import ccall "Clp_getUserPointer"                getUserPointer                :: ClpSimplex -> IO (Ptr ())
-- foreign import ccall "Clp_registerCallBack" registerCallBack :: ClpSimplex -> <clp_callback> -> IO ()
foreign import ccall "Clp_clearCallBack"                 clearCallBack                 :: ClpSimplex -> IO ()
foreign import ccall "Clp_setLogLevel"                   setLogLevel                   :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_logLevel"                      logLevel                      :: ClpSimplex -> IO CInt
foreign import ccall "Clp_lengthNames"                   lengthNames                   :: ClpSimplex -> IO CInt
foreign import ccall "Clp_rowName"                       rowName                       :: ClpSimplex -> CInt -> CString -> IO ()
foreign import ccall "Clp_columnName"                    columnName                    :: ClpSimplex -> CInt -> CString -> IO ()
foreign import ccall "Clp_initialSolve"                  initialSolve                  :: ClpSimplex -> IO CInt
foreign import ccall "Clp_initialSolveWithOptions"       initialSolveWithOptions       :: ClpSimplex -> ClpSolve -> IO CInt
foreign import ccall "Clp_initialDualSolve"              initialDualSolve              :: ClpSimplex -> IO CInt
foreign import ccall "Clp_initialPrimalSolve"            initialPrimalSolve            :: ClpSimplex -> IO CInt
foreign import ccall "Clp_initialBarrierSolve"           initialBarrierSolve           :: ClpSimplex -> IO CInt
foreign import ccall "Clp_initialBarrierNoCrossSolve"    initialBarrierNoCrossSolve    :: ClpSimplex -> IO CInt
foreign import ccall "Clp_dual"                          dual                          :: ClpSimplex -> CInt -> IO CInt
foreign import ccall "Clp_primal"                        primal                        :: ClpSimplex -> CInt -> IO CInt
foreign import ccall "Clp_idiot"                         idiot                         :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_scaling"                       scaling                       :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_scalingFlag"                   scalingFlag                   :: ClpSimplex -> IO CInt
foreign import ccall "Clp_crash"                         crash                         :: ClpSimplex -> CDouble -> CInt -> IO CInt
foreign import ccall "Clp_primalFeasible"                primalFeasible                :: ClpSimplex -> IO CInt
foreign import ccall "Clp_dualFeasible"                  dualFeasible                  :: ClpSimplex -> IO CInt
foreign import ccall "Clp_dualBound"                     dualBound                     :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setDualBound"                  setDualBound                  :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_infeasibilityCost"             infeasibilityCost             :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setInfeasibilityCost"          setInfeasibilityCost          :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_perturbation"                  perturbation                  :: ClpSimplex -> IO CInt
foreign import ccall "Clp_setPerturbation"               setPerturbation               :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_algorithm"                     algorithm                     :: ClpSimplex -> IO CInt
foreign import ccall "Clp_setAlgorithm"                  setAlgorithm                  :: ClpSimplex -> CInt -> IO ()
foreign import ccall "Clp_sumDualInfeasibilities"        sumDualInfeasibilities        :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_numberDualInfeasibilities"     numberDualInfeasibilities     :: ClpSimplex -> IO CInt
foreign import ccall "Clp_sumPrimalInfeasibilities"      sumPrimalInfeasibilities      :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_numberPrimalInfeasibilities"   numberPrimalInfeasibilities   :: ClpSimplex -> IO CInt
foreign import ccall "Clp_saveModel"                     saveModel                     :: ClpSimplex -> CString -> IO CInt
foreign import ccall "Clp_restoreModel"                  restoreModel                  :: ClpSimplex -> CString -> IO CInt
foreign import ccall "Clp_checkSolution"                 checkSolution                 :: ClpSimplex -> IO ()
foreign import ccall "Clp_getNumRows"                    getNumRows                    :: ClpSimplex -> IO CInt
foreign import ccall "Clp_getNumCols"                    getNumCols                    :: ClpSimplex -> IO CInt
foreign import ccall "Clp_getIterationCount"             getIterationCount             :: ClpSimplex -> IO CInt
foreign import ccall "Clp_isAbandoned"                   isAbandoned                   :: ClpSimplex -> IO CInt
foreign import ccall "Clp_isProvenOptimal"               isProvenOptimal               :: ClpSimplex -> IO CInt
foreign import ccall "Clp_isProvenPrimalInfeasible"      isProvenPrimalInfeasible      :: ClpSimplex -> IO CInt
foreign import ccall "Clp_isProvenDualInfeasible"        isProvenDualInfeasible        :: ClpSimplex -> IO CInt
foreign import ccall "Clp_isPrimalObjectiveLimitReached" isPrimalObjectiveLimitReached :: ClpSimplex -> IO CInt
foreign import ccall "Clp_isDualObjectiveLimitReached"   isDualObjectiveLimitReached   :: ClpSimplex -> IO CInt
foreign import ccall "Clp_isIterationLimitReached"       isIterationLimitReached       :: ClpSimplex -> IO CInt
foreign import ccall "Clp_getObjSense"                   getObjSense                   :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setObjSense"                   setObjSense                   :: ClpSimplex -> CDouble -> IO ()
foreign import ccall "Clp_getRowActivity"                getRowActivity                :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getColSolution"                getColSolution                :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_setColSolution"                setColSolution                :: ClpSimplex -> Ptr CDouble -> IO ()
foreign import ccall "Clp_getRowPrice"                   getRowPrice                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getReducedCost"                getReducedCost                :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getRowLower"                   getRowLower                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getRowUpper"                   getRowUpper                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getObjCoefficients"            getObjCoefficients            :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getColLower"                   getColLower                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getColUpper"                   getColUpper                   :: ClpSimplex -> IO (Ptr CDouble)
foreign import ccall "Clp_getObjValue"                   getObjValue                   :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_printModel"                    printModel                    :: ClpSimplex -> CString -> IO ()
foreign import ccall "Clp_getSmallElementValue"          getSmallElementValue          :: ClpSimplex -> IO CDouble
foreign import ccall "Clp_setSmallElementValue"          setSmallElementValue          :: ClpSimplex -> CDouble -> IO ()


--- * Clp Solve ------------------------------------------------------------------------------------------------------

foreign import ccall "ClpSolve_new"                      newSolver                     :: IO ClpSolve
foreign import ccall "ClpSolve_delete"                   deleteSolver                  :: ClpSolve -> IO ()

foreign import ccall "ClpSolve_setSpecialOption"         setSpecialOption              :: ClpSolve -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "ClpSolve_getSpecialOption"         getSpecialOption              :: ClpSolve -> CInt -> IO CInt
foreign import ccall "ClpSolve_setSolveType"             setSolveType                  :: ClpSolve -> CInt -> CInt -> IO ()
foreign import ccall "ClpSolve_getSolveType"             getSolveType                  :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setPresolveType"          setPresolveType               :: ClpSolve -> CInt -> CInt -> IO ()
foreign import ccall "ClpSolve_getPresolveType"          getPresolveType               :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_getPresolvePasses"        getPresolvePasses             :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_getExtraInfo"             getExtraInfo                  :: ClpSolve -> CInt -> IO CInt
foreign import ccall "ClpSolve_setInfeasibleReturn"      setInfeasibleReturn           :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_infeasibleReturn"         infeasibleReturn              :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_doDual"                   doDual                        :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoDual"                setDoDual                     :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doSingleton"              doSingleton                   :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoSingleton"           setDoSingleton                :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doDoubleton"              doDoubleton                   :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoDoubleton"           setDoDoubleton                :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doTripleton"              doTripleton                   :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoTripleton"           setDoTripleton                :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doTighten"                doTighten                     :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoTighten"             setDoTighten                  :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doForcing"                doForcing                     :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoForcing"             setDoForcing                  :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doImpliedFree"            doImpliedFree                 :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoImpliedFree"         setDoImpliedFree              :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doDupcol"                 doDupcol                      :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoDupcol"              setDoDupcol                   :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doDuprow"                 doDuprow                      :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoDuprow"              setDoDuprow                   :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_doSingletonColumn"        doSingletonColumn             :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setDoSingletonColumn"     setDoSingletonColumn          :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_presolveActions"          presolveActions               :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setPresolveActions"       setPresolveActions            :: ClpSolve -> CInt -> IO ()
foreign import ccall "ClpSolve_substitution"             substitution                  :: ClpSolve -> IO CInt
foreign import ccall "ClpSolve_setSubstitution"          setSubstitution               :: ClpSolve -> CInt -> IO ()


--- * finalizer ------------------------------------------------------------------------------------------------------

foreign import ccall "&Clp_deleteModel"                   deleteModelF                 :: FunPtr (ClpSimplex -> IO ())
foreign import ccall "&ClpSolve_delete"                   deleteSolverF                :: FunPtr (ClpSolve -> IO ())

