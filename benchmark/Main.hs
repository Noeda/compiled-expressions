module Main where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Criterion
import Criterion.Main
import Data.CompiledExpression

lst100 :: (Enum a, Floating a) => [a]
lst100 = [0..99]

lst1000 :: (Enum a, Floating a) => [a]
lst1000 = [0..999]

lst10000 :: (Enum a, Floating a) => [a]
lst10000 = [0..9999]

sinProgram :: (Floating a, Monad m) => a -> StateT a m ()
sinProgram initial = do
    replicateM_ 2837 $ do
        old <- get
        put (old + sin (old+initial+cos old))
{-# NOINLINE sinProgram #-}

runSinProgram :: Floating a => a -> a
runSinProgram val = execState (sinProgram val) 0

reductingProgram :: Floating a => [a] -> [a]
reductingProgram = fmap (const 5)

inputMapProgram :: Floating a => [a] -> [a]
inputMapProgram = fmap (**2)

main :: IO ()
main =
   let lst100sum = compileExpression1 traverse sum (lst100 :: [Double])
       lst1000sum = compileExpression1 traverse sum (lst1000 :: [Double])
       lst10000sum = compileExpression1 traverse sum (lst10000 :: [Double])
       runSinProgramCompiled = compileExpression3 runSinProgram
       reductingCompiled = compileExpression traverse traverse reductingProgram (replicate 10 ())
       inputMapCompiled = compileExpression traverse traverse inputMapProgram (replicate 10 ())
    in defaultMain [
         bench "sinProgram (vanilla)" $ whnf runSinProgram (123 :: Double)
       , bench "sinProgram (compiled)" $ whnf runSinProgramCompiled 123
       , bgroup "sums of lists (vanilla should be faster)" [
           bench "sum of 100 doubles (vanilla)" $ whnf sum (lst100 :: [Double])
         , bench "sum of 100 doubles (compiled)" $ whnf lst100sum lst100
         , bench "sum of 1000 doubles (vanilla)" $ whnf sum (lst1000 :: [Double])
         , bench "sum of 1000 doubles (compiled)" $ whnf lst1000sum lst1000
         , bench "sum of 10000 doubles (vanilla)" $ whnf sum (lst10000 :: [Double])
         , bench "sum of 10000 doubles (compiled)" $ whnf lst10000sum lst10000
         ]
       , bench "reducting program" $ whnf reductingCompiled [1,2,3,4,5,6,7,8,9,10]
       , bench "input mapping program" $ whnf inputMapCompiled [11,22,33,44,55]
       ]

