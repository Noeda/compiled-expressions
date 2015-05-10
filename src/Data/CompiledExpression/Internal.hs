{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Data.CompiledExpression.Internal where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens hiding ( Context )
import Control.Monad ( void, forever, unless, when )
import Control.Monad.IO.Class
import Control.Monad.Primitive ( touch )
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Hashable
import GHC.Generics hiding ( moduleName )
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import LLVM.General as G
import LLVM.General.Threading as G
import LLVM.General.AST as LLVM hiding ( mask, type' )
import LLVM.General.AST.AddrSpace as LLVM
import LLVM.General.AST.Linkage as LLVM
import LLVM.General.AST.CallingConvention as LLVM
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as C
import LLVM.General.AST.FloatingPointPredicate as LLVM hiding ( False, True )
import qualified LLVM.General.AST.IntegerPredicate as LLVM
import LLVM.General.AST.Global as LLVM
import LLVM.General.Context as G
import LLVM.General.ExecutionEngine as G
import System.Mem.StableName
import System.IO.Unsafe ( unsafePerformIO )

type FloatFun = Ptr CDouble -> Ptr CDouble -> IO ()

foreign import ccall unsafe "dynamic" mkCalling :: FunPtr FloatFun -> FloatFun

debugTrace :: String -> a -> a
#ifdef SPRINKLE_DEBUG_MESSAGES
debugTrace str thing = unsafePerformIO $ putStrLn str >> return thing
#else
debugTrace _ thing = thing
#endif
{-# INLINE debugTrace #-}

debugTraceShow :: Show b => b -> a -> a
debugTraceShow showable thing = debugTrace (show showable) thing
{-# INLINE debugTraceShow #-}

-- | Type of deep embedding of expressions.
--
-- Note that `Eq` and `Ord` instances for this type don't behave like they
-- would behave on numbers.
data SynthVarG a
    = Source !a
    | Negate (SynthVarG a)
    | Absolute (SynthVarG a)
    | Signum (SynthVarG a)
    | Constant !Double
    | Plus (SynthVarG a) (SynthVarG a)
    | Minus (SynthVarG a) (SynthVarG a)
    | Multiply (SynthVarG a) (SynthVarG a)
    | Divide (SynthVarG a) (SynthVarG a)
    | Exp (SynthVarG a)
    | Log (SynthVarG a)
    | Sqrt (SynthVarG a)
    | Power (SynthVarG a) (SynthVarG a)
    | Sin (SynthVarG a)
    | Cos (SynthVarG a)
    | Tan (SynthVarG a)
    | Asin (SynthVarG a)
    | Acos (SynthVarG a)
    | Atan (SynthVarG a)
    | Sinh (SynthVarG a)
    | Cosh (SynthVarG a)
    | Tanh (SynthVarG a)
    | Asinh (SynthVarG a)
    | Acosh (SynthVarG a)
    | Atanh (SynthVarG a)
    deriving ( Eq, Ord, Show, Data, Generic, Typeable, Functor, Foldable, Traversable )

data CodeGenState = CodeGenState
    { _visitedNodes :: !IS.IntSet
    , _outputInstructions :: ![Named LLVM.Instruction] }
makeLenses ''CodeGenState

type SynthVar = SynthVarG Int

instance Hashable a => Hashable (SynthVarG a)

source :: Int -> SynthVar
source = Source

type SynthRef = Int

data SynthVarGraph
    = GShared !Int
    | GSource !Int
    | GNegate SynthRef
    | GAbsolute SynthRef
    | GSignum SynthRef
    | GConstant !Double
    | GPlus SynthRef SynthRef
    | GMinus SynthRef SynthRef
    | GMultiply SynthRef SynthRef
    | GDivide SynthRef SynthRef
    | GExp SynthRef
    | GLog SynthRef
    | GSqrt SynthRef
    | GPower SynthRef SynthRef
    | GSin SynthRef
    | GCos SynthRef
    | GTan SynthRef
    | GAsin SynthRef
    | GAcos SynthRef
    | GAtan SynthRef
    | GSinh SynthRef
    | GCosh SynthRef
    | GTanh SynthRef
    | GAsinh SynthRef
    | GAcosh SynthRef
    | GAtanh SynthRef
    deriving ( Show, Data, Generic, Typeable )

newtype SN = SN (StableName SynthVar)
    deriving ( Eq, Typeable )

instance Hashable SN where
    hashWithSalt salt (SN stable_name) =
        hashStableName stable_name `xor` salt

data SynthVarBuilder = SynthVarBuilder
    { _nextIndex :: !Int
    , _stableNameMap :: !(HM.HashMap SN Int)
    , _duplicateCheckMap :: !(M.Map SynthVar Int)
    , _graph :: !(IM.IntMap SynthVarGraph)
#ifdef SPRINKLE_DEBUG_MESSAGES
    , _numStableNameReductions :: !Int
    , _numEqualityReductions :: !Int
#endif
    }
    deriving ( Typeable, Generic )
makeLenses ''SynthVarBuilder

areEqualIgnoringInputs :: SynthVar -> SynthVar -> Bool
areEqualIgnoringInputs svar1 svar2 =
    let inputless1 = fmap (const ()) svar1
        inputless2 = fmap (const ()) svar2
     in inputless1 == inputless2

initialSynthVarBuilder :: SynthVarBuilder
initialSynthVarBuilder = SynthVarBuilder
    { _nextIndex = 0
    , _stableNameMap = HM.empty
    , _duplicateCheckMap = M.empty
    , _graph = IM.empty
#ifdef SPRINKLE_DEBUG_MESSAGES
    , _numStableNameReductions = 0
    , _numEqualityReductions = 0
#endif
    }

sourcify :: forall a f. Traversable f
         => f a -> f SynthVar
sourcify structure =
    evalState (traverse sourceIndex structure) 0
  where
    sourceIndex :: a -> State Int SynthVar
    sourceIndex _ = do
        idx <- get
        put (idx+1)
        return $ Source idx

combine :: Either a a -> a
combine (Left x) = x
combine (Right x) = x

globalJit :: TMVar (Maybe (JIT, Context))
globalJit = unsafePerformIO $ newTMVarIO Nothing
{-# NOINLINE globalJit #-}

jitCreated :: MVar Bool
jitCreated = unsafePerformIO $ newMVar False
{-# NOINLINE jitCreated #-}

jitThread :: MVar () -> IO ()
jitThread ready_mvar = mask $ \restore -> do
    setMultithreaded True
    withContext $ \ctx -> withJIT ctx 0 $ \jit -> do
        i_own_context <- atomically $ do
            result <- readTMVar globalJit
            case result of
                Nothing -> do
                    void $ takeTMVar globalJit
                    putTMVar globalJit (Just (jit, ctx)) >> return True
                Just _ -> return False
        when i_own_context $ do
            putMVar ready_mvar ()
            restore $ forever $ threadDelay 100000000 >> touch jit >> touch ctx

ensureJitExists :: IO ()
ensureJitExists = mask_ $ do
    x <- takeMVar jitCreated
    if x
      then putMVar jitCreated True
      else do mvar <- newEmptyMVar
              void $ forkIO $ jitThread mvar
              takeMVar mvar
              putMVar jitCreated True

withJitAccess :: (JIT -> Context -> IO () -> IO () -> IO a) -> IO a
withJitAccess action = do
    ensureJitExists
    mask $ \restore -> do
        is_it_taken_tvar <- newTVarIO True
        Just (jit, ctx) <- atomically $
            takeTMVar globalJit
        let maybeRestore = atomically $ do
                               is_it_taken <- readTVar is_it_taken_tvar
                               when is_it_taken $ do
                                   writeTVar is_it_taken_tvar False
                                   putTMVar globalJit (Just (jit, ctx))

            maybeTake = atomically $ do
                            is_it_taken <- readTVar is_it_taken_tvar
                            unless is_it_taken $ do
                                writeTVar is_it_taken_tvar True
                                void $ takeTMVar globalJit

        finally (restore $ action jit ctx maybeRestore maybeTake) maybeRestore

llvmCode :: LLVM.Module -> IO (Ptr CDouble -> Ptr CDouble -> IO ())
llvmCode mod = do
    result_mvar <- newEmptyMVar
    void $ forkIO $
        mask $ \restore -> withJitAccess $ \jit ctx restoreJit takeJit ->
            do_it restore result_mvar jit ctx restoreJit takeJit
    takeMVar result_mvar
  where
    do_it restore result_mvar jit ctx restoreJit takeJit = do
        result <- runExceptT $ withModuleFromAST ctx mod $ \gmod -> do
            withModuleInEngine jit gmod $ \execmodule -> do
                () <- restoreJit
                flip finally takeJit $ do
                    funptr <- getFunction execmodule (Name "synthvargraph_computation")
                    case funptr of
                        Nothing -> error "llvmCode: cannot execute JIT."
                        Just actual_ptr -> do
                            let addr = mkCalling (castFunPtr actual_ptr)
                            addr nullPtr nullPtr

                            my_tid <- myThreadId
                            finalizer_mvar <- newEmptyMVar
                            void $ mkWeakMVar finalizer_mvar $
                                killThread my_tid

                            putMVar result_mvar $ \ptr1 ptr2 -> do
                                addr ptr1 ptr2
                                touch finalizer_mvar

                            restore $ forever $ threadDelay 100000000
        case result of
            Left err -> error err
            Right ok -> return ok
{-# INLINE llvmCode #-}

-- | Given a function that computes some operation between two `Traversable`
-- containers and a structure of the container, returns an optimized function
-- that (hopefully) computes it faster.
--
-- Behind the scenes, the expression is transformed to LLVM IR, compiled to
-- machine code and then returned in a form callable from Haskell.
--
-- There is a lot of overhead in calling the returned function. If your Haskell
-- function is a simple one that GHC can already optimize well then the
-- returned function has a high chance of being slower, perhaps significanty so.
-- However, if your function is a complex one that GHC either has trouble
-- optimizing (e.g. you are using complex data structures or functions from
-- many libraries) or cannot optimize, then `compileExpression` might be able
-- to help you.
--
-- Depending on the size of your computation, this function can take a long
-- time to return.
--
-- The first two arguments traverse the containers (you might want to check the
-- \'lens\' package). If your containers implement `Traversable`, then you can
-- just use `traverse` here directly.
--
-- The third argument to this function is the Haskell function to be optimized.
--
-- The fourth argument gives the structure of the container that
-- `compileExpression` will assume is used for any future computation. For
-- example, if you call `compileExpression` with a list of 5 elements:
--
-- @
--     let optimized = compileExpression traverse traverse (\lst -> [sum lst]) [(), (), (), (), ()]
-- @
--
-- Then subsequently, the returned function \'optimized\' will only work with
-- lists of 5 elements:
--
-- @
--     optimized [1,2,3,4,5]    -- ok, returns 1+2+3+4+5 = 15
--     optimized [4,3,2,1]      -- not ok, throws an error
-- @
--
-- The structure is checked for the number of elements but not further than
-- that.
compileExpression :: (forall c d. Traversal (f c) (f d) c d)
                  -> (forall c d. Traversal (f2 c) (f2 d) c d)
                  -> (forall a. Floating a => f a -> f2 a)
                  -> f b   -- ^ Structure of the container. The values are not used.
                  -> (f Double -> f2 Double)
compileExpression traverse1 traverse2 modifier source = unsafePerformIO $ do
    let walked = modifier synthesized
    (outputs, graphized) <- graphifyStructure traverse2 walked

    let listed_outputs = toListOf traverse2 outputs
        mod = llvmfy (listed_outputs, graphized)
        num_outputs = length listed_outputs
        stumped_output = stump walked

    raw_function <- debugTrace ("Compiling a function with " <>
                                show num_outputs <> " outputs and " <>
                                show len <> " inputs. Graph size " <> show (IM.size graphized)) $
                    llvmCode mod

    return $ \inp -> unsafePerformIO $ do
        let inp_len = lengthOf traverse1 inp
        unless (inp_len == len) $
            error $ "<expression compiled by compileExpression>: Input length is unexpected. Expecting length " <> show len <> ", got " <> show inp_len <> "."

        allocaArray inp_len $ \inp_arr -> do
            iforOf_ (indexing traverse1) inp $ \index value ->
                pokeElemOff inp_arr index (CDouble value)
            allocaArray num_outputs $ \(output_arr :: Ptr CDouble) -> do
                raw_function inp_arr output_arr
                unwrap stumped_output (castPtr output_arr)
  where
    stump = over traverse2 (const ())

    unwrap structure ptr = evalStateT (traverseOf traverse2 unwrapItem structure) 0
      where
        unwrapItem _ = do
            idx <- get
            put (idx+1)
            result <- liftIO $ peekElemOff ptr idx
            return result

    (synthesized, len) = runState (traverseOf traverse1 walk source) 0
      where
        walk _ = do
            idx <- get
            put (idx+1)
            return $ Source idx
{-# INLINE compileExpression #-}

-- | Given a traversable container of `SynthVar`s, build a `SynthVarGraph`.
graphifyStructure :: (forall a b. Traversal (f a) (f b) a b)
                  -> f SynthVar
                  -> IO (f Int, IM.IntMap SynthVarGraph) -- ^ Returns the indices of output variables and the variable graph.
graphifyStructure traversal1 structure = do
    (result, svb) <- flip runStateT initialSynthVarBuilder $ forOf traversal1 structure $ \synth_var -> do
        old_svb <- get
        (target_int, new_svb) <- liftIO $ graphify synth_var old_svb
        put new_svb
        return target_int
    do
#ifdef SPRINKLE_DEBUG_MESSAGES
        debugTrace ("Made " <> show (svb^.numStableNameReductions) <> " stable name reductions and " <> show (svb^.numEqualityReductions) <> " equality reductions in this compilation.") $
#endif
            return (result, svb^.graph)

{-# INLINE graphifyStructure #-}

llvmfy :: ([Int], IM.IntMap SynthVarGraph) -> LLVM.Module
llvmfy (outs, graph) = defaultModule { moduleName = "synthvargraph"
                                     , moduleDefinitions = defs }
  where
    float_ptr_type = PointerType float_type (AddrSpace 0)
    float_type = FloatingPointType 64 IEEE

    input_vector_name = Name "input_vector"
    output_vector_name = Name "output_vector"

    defs = mathDecls ++ [bodyDecl]

    mathDecls =
        [fun "pow" 2
        ,fun "fabs" 1
        ,fun "exp" 1
        ,fun "log" 1
        ,fun "sqrt" 1
        ,fun "sin" 1
        ,fun "cos" 1
        ,fun "tan" 1
        ,fun "asin" 1
        ,fun "acos" 1
        ,fun "atan" 1
        ,fun "sinh" 1
        ,fun "cosh" 1
        ,fun "tanh" 1
        ,fun "asinh" 1
        ,fun "acosh" 1
        ,fun "atanh" 1
        ]
      where
        fun name c = ext name (FunctionType float_type (replicate c float_type) False)

        ext name t = GlobalDefinition $ globalVariableDefaults {
            linkage = External
          , LLVM.type' = t
          , name = Name name }

    bodyDecl =
           GlobalDefinition $ functionDefaults {
        name = Name "synthvargraph_computation"
      , parameters = ([Parameter float_ptr_type input_vector_name []
                      ,Parameter float_ptr_type output_vector_name []]
                     ,False)
      , returnType = VoidType
      , basicBlocks = [BasicBlock (Name "check1") checkBody1 (Do $ CondBr (LocalReference (IntegerType 1) (Name "check1"))
                      (Name "bailout")
                      (Name "check2") [])

                      ,BasicBlock (Name "check2") checkBody2 (Do $ CondBr (LocalReference (IntegerType 1) (Name "check2"))
                      (Name "bailout")
                      (Name "block") [])
                      ,BasicBlock (Name "block") body (Do (Ret Nothing []))
                      ,BasicBlock (Name "bailout") [] (Do (Ret Nothing []))] }

    checkBody1 =
        [ Name "check1" := ICmp LLVM.EQ (LocalReference float_ptr_type input_vector_name) (ConstantOperand (C.Null float_ptr_type)) [] ]

    checkBody2 =
        [ Name "check2" := ICmp LLVM.EQ (LocalReference float_ptr_type input_vector_name) (ConstantOperand (C.Null float_ptr_type)) [] ]

    body = reverse (_outputInstructions (execState (traverse_ bodyBuilder (IM.assocs graph)) (CodeGenState IS.empty []))) ++ body_output

    body_output = concat $ fmap outBuilder $ zip [(0 :: Int)..] outs

    outBuilder (idx, key) =
        [tmp_name := GetElementPtr False (LocalReference float_ptr_type output_vector_name) [ConstantOperand (C.Int 32 (fromIntegral idx))] []
        ,Do $ Store False (LocalReference float_ptr_type tmp_name) (LocalReference float_type inp_name) Nothing 0 []]
      where
        inp_name = Name ("i" ++ show key)
        tmp_name = Name ("outtmp" ++ show key)

    bodyBuilder (key, value) = do
        visited <- use visitedNodes
        if IS.member key visited
          then return ()
          else do visitedNodes %= IS.insert key
                  result <- instr
                  outputInstructions %= ((reverse result) ++)
      where
        recurse idx =
            let value = fromMaybe (error "invalid index") $ IM.lookup idx graph
             in bodyBuilder (idx, value)

        instr = case value of
            GShared index -> do
                recurse index
                return
                    [result_name := Select (ConstantOperand (C.Int 1 1))
                                           (local_ref_float index)
                                           (local_ref_float index)
                                           []]
            GSource index ->
                return [addr_name := GetElementPtr False (LocalReference float_ptr_type input_vector_name) [ConstantOperand (C.Int 32 (fromIntegral index))] []
                       ,result_name := Load False (LocalReference float_ptr_type addr_name) Nothing 0 []]
            GConstant value ->
                return [result_name := Select (ConstantOperand (C.Int 1 1))
                                              (constant value)
                                              (constant value)
                                              []]
            GNegate index -> do
                recurse index
                return [result_name := FMul fmf (ConstantOperand (C.Float (C.Double (-1))))
                                                (local_ref_float index)
                                               []]
            GAbsolute index -> do
                recurse index
                return $ call "fabs" 1 [index]
            GSignum index -> do
                recurse index
                return [compare_name1 := FCmp OLT (local_ref_float index) (constant 0) []
                       ,compare_name2 := FCmp OGT (local_ref_float index) (constant 0) []
                       ,temp1_name := Select (LocalReference (IntegerType 1) compare_name1)
                                       (constant (-1))
                                       (constant 0) []
                       ,temp2_name := Select (LocalReference (IntegerType 1) compare_name2)
                                       (constant 1)
                                       (constant 0) []
                       ,result_name := FAdd fmf
                           (LocalReference float_type temp1_name)
                           (LocalReference float_type temp2_name) []]
            GPlus index1 index2 -> do
                recurse index1
                recurse index2
                return [result_name := FAdd fmf (local_ref_float index1)
                                                (local_ref_float index2)
                                                []]
            GMinus index1 index2 -> do
                recurse index1
                recurse index2
                return [result_name := FSub fmf (local_ref_float index1)
                                                (local_ref_float index2)
                                                []]
            GMultiply index1 index2 -> do
                recurse index1
                recurse index2
                return [result_name := FMul fmf (local_ref_float index1)
                                                (local_ref_float index2)
                                                []]

            GDivide index1 index2 -> do
                recurse index1
                recurse index2
                return [result_name := FDiv fmf (local_ref_float index1)
                                                (local_ref_float index2)
                                                []]

            GPower index1 index2 -> do
                recurse index1
                recurse index2
                return $ call "pow" 2 [index1, index2]

            GExp index -> do
                recurse index
                return $ call "exp" 1 [index]

            GLog index -> do
                recurse index
                return $ call "log" 1 [index]

            GSqrt index -> do
                recurse index
                return $ call "sqrt" 1 [index]

            GSin index -> do
                recurse index
                return $ call "sin" 1 [index]

            GCos index -> do
                recurse index
                return $ call "cos" 1 [index]

            GTan index -> do
                recurse index
                return $ call "tan" 1 [index]

            GAsin index -> do
                recurse index
                return $ call "asin" 1 [index]

            GAcos index -> do
                recurse index
                return $ call "acos" 1 [index]

            GAtan index -> do
                recurse index
                return $ call "atan" 1 [index]

            GSinh index -> do
                recurse index
                return $ call "sinh" 1 [index]

            GCosh index -> do
                recurse index
                return $ call "cosh" 1 [index]

            GTanh index -> do
                recurse index
                return $ call "tanh" 1 [index]

            GAsinh index -> do
                recurse index
                return $ call "asinh" 1 [index]

            GAcosh index -> do
                recurse index
                return $ call "acosh" 1 [index]

            GAtanh index -> do
                recurse index
                return $ call "atanh" 1 [index]


        call name count args = [result_name := Call False C []
                (Right (ConstantOperand (C.GlobalReference (FunctionType float_type (replicate count float_type) False) (Name name))))
                (fmap (\x -> (local_ref_float x, [])) args) [] []]

        fmf = NoFastMathFlags
        constant x = ConstantOperand (C.Float (C.Double x))

        ref_name key = Name ("i" ++ show key)
        local_ref_float key = LocalReference float_type (ref_name key)

        compare_name1 = Name ("i" ++ show key ++ "cmp1")
        compare_name2 = Name ("i" ++ show key ++ "cmp2")
        temp1_name = Name ("i" ++ show key ++ "temp1")
        temp2_name = Name ("i" ++ show key ++ "temp2")

        addr_name = Name ("i" ++ show key ++ "addr")
        result_name = Name ("i" ++ show key)


graphify :: SynthVar -> SynthVarBuilder -> IO (Int, SynthVarBuilder)
graphify sv svb = runStateT (walk sv) svb

walk :: SynthVar -> StateT SynthVarBuilder IO Int
walk sv = do
    idx <- use nextIndex
    nextIndex += 1
    sv' <- liftIO $ evaluate sv
    -- Try finding a copy by stable name (fast)
    sn <- SN <$> liftIO (makeStableName sv')
    use (stableNameMap.at sn) >>= \case
        Just old_idx -> do
#ifdef SPRINKLE_DEBUG_MESSAGES
            numStableNameReductions += 1
#endif
            graph.at idx .= Just (GShared old_idx)
        _ -> do
            -- Try finding a copy by a Data.Map value (slower)
            use (duplicateCheckMap.at sv') >>= \case
                Just old_idx -> do
#ifdef SPRINKLE_DEBUG_MESSAGES
                    numEqualityReductions += 1
#endif
                    graph.at idx .= Just (GShared old_idx)
                _ -> do
                    stableNameMap.at sn .= Just idx
                    duplicateCheckMap.at sv' .= Just idx
                    graphified <- graphify sv'
                    graphified' <- liftIO $ evaluate graphified
                    graph.at idx .= Just graphified'
    return idx
  where
    graphify (Source s1) =
        let gs = GSource s1
         in gs `seq` (return gs)
    graphify (Negate s1) = GNegate <$> walk s1
    graphify (Absolute s1) = GAbsolute <$> walk s1
    graphify (Signum s1) = GSignum <$> walk s1
    graphify (Constant s1) =
        let gs = GConstant s1
         in gs `seq` (return gs)
    graphify (Plus s1 s2) = GPlus <$> walk s1 <*> walk s2
    graphify (Minus s1 s2) = GMinus <$> walk s1 <*> walk s2
    graphify (Multiply s1 s2) = GMultiply <$> walk s1 <*> walk s2
    graphify (Divide s1 s2) = GDivide <$> walk s1 <*> walk s2
    graphify (Power s1 s2) = GPower <$> walk s1 <*> walk s2
    graphify (Sqrt s1) = GSqrt <$> walk s1
    graphify (Log s1) = GLog <$> walk s1
    graphify (Exp s1) = GExp <$> walk s1
    graphify (Cos s1) = GCos <$> walk s1
    graphify (Sin s1) = GSin <$> walk s1
    graphify (Tan s1) = GTan <$> walk s1
    graphify (Acos s1) = GAcos <$> walk s1
    graphify (Asin s1) = GAsin <$> walk s1
    graphify (Atan s1) = GAtan <$> walk s1
    graphify (Cosh s1) = GCosh <$> walk s1
    graphify (Sinh s1) = GSinh <$> walk s1
    graphify (Tanh s1) = GTanh <$> walk s1
    graphify (Acosh s1) = GAcosh <$> walk s1
    graphify (Asinh s1) = GAsinh <$> walk s1
    graphify (Atanh s1) = GAtanh <$> walk s1

synthVarSize :: SynthVar -> Int
synthVarSize (Source {}) = 1
synthVarSize (Negate sz) = 1 + synthVarSize sz
synthVarSize (Absolute sz) = 1 + synthVarSize sz
synthVarSize (Signum sz) = 1 + synthVarSize sz
synthVarSize (Constant {}) = 1
synthVarSize (Plus sz1 sz2) = 1 + synthVarSize sz1 + synthVarSize sz2
synthVarSize (Minus sz1 sz2) = 1 + synthVarSize sz1 + synthVarSize sz2
synthVarSize (Multiply sz1 sz2) = 1 + synthVarSize sz1 + synthVarSize sz2
synthVarSize (Divide sz1 sz2) = 1 + synthVarSize sz1 + synthVarSize sz2
synthVarSize (Exp sz) = 1 + synthVarSize sz
synthVarSize (Log sz) = 1 + synthVarSize sz
synthVarSize (Sqrt sz) = 1 + synthVarSize sz
synthVarSize (Power sz1 sz2) = 1 + synthVarSize sz1 + synthVarSize sz2
synthVarSize (Sin sz) = 1 + synthVarSize sz
synthVarSize (Cos sz) = 1 + synthVarSize sz
synthVarSize (Tan sz) = 1 + synthVarSize sz
synthVarSize (Asin sz) = 1 + synthVarSize sz
synthVarSize (Acos sz) = 1 + synthVarSize sz
synthVarSize (Atan sz) = 1 + synthVarSize sz
synthVarSize (Sinh sz) = 1 + synthVarSize sz
synthVarSize (Cosh sz) = 1 + synthVarSize sz
synthVarSize (Tanh sz) = 1 + synthVarSize sz
synthVarSize (Asinh sz) = 1 + synthVarSize sz
synthVarSize (Acosh sz) = 1 + synthVarSize sz
synthVarSize (Atanh sz) = 1 + synthVarSize sz

synthVarize :: Functor f => f Double -> f SynthVar
synthVarize = fmap Constant

instance Num SynthVar where
    negate = Negate
    n1 + n2 = Plus n1 n2
    n1 - n2 = Minus n1 n2
    n1 * n2 = Multiply n1 n2
    fromInteger = Constant . fromInteger
    abs = Absolute
    signum = Signum

instance Fractional SynthVar where
    n1 / n2 = Divide n1 n2
    fromRational ratio = Constant $ fromRational ratio

instance Floating SynthVar where
    pi = Constant pi
    exp n1 = Exp n1
    log = Log
    sqrt = Sqrt
    n1 ** n2 = Power n1 n2
    sin = Sin
    cos = Cos
    tan = Tan
    asin = Asin
    acos = Acos
    atan = Atan
    sinh = Sinh
    cosh = Cosh
    tanh = Tanh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh


