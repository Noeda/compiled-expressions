{-# LANGUAGE RankNTypes #-}

module Data.CompiledExpression
    ( -- * Core functionality
      compileExpression
      -- * Utilities
      --
      -- This functions are implemented in terms of `compileExpression`
    , compileExpression1
    , compileExpression2
    , compileExpression3
      -- * Conversion between numeric types
    , realToFloating )
    where

import Control.Lens
import Data.CompiledExpression.Internal

-- | Same as `compileExpression` but does not use a container in output.
compileExpression1 :: (forall a b. Traversal (f a) (f b) a b)
                   -> (forall a. Floating a => f a -> a)
                   -> f b
                   -> (f Double -> Double)
compileExpression1 traverse1 fun source =
    let result_fun = compileExpression traverse1 traverse (\input -> Identity (fun input)) source
     in \input -> runIdentity (result_fun input)

-- | Same as `compileExpression` but does not use a container in input.
compileExpression2 :: (forall a b. Traversal (f a) (f b) a b)
                   -> (forall a. Floating a => a -> f a)
                   -> (Double -> f Double)
compileExpression2 traverse2 fun =
    let result_fun = compileExpression traverse traverse2
                                       (\(Identity input) -> fun input)
                                       (Identity ())
     in \input -> result_fun (Identity input)

-- | Same as `compileExpression` but does not use containers at all, just input
-- and output.
compileExpression3 :: (forall a. Floating a => a -> a)
                   -> (Double -> Double)
compileExpression3 fun =
    let result_fun = compileExpression traverse traverse
                                       (\(Identity input) -> Identity (fun input))
                                       (Identity ())
     in \input -> runIdentity (result_fun (Identity input))

-- | Converts between a `Real` into a `Floating` value.
--
-- You may find this useful to embed non-literal constants into expressions
-- that are being compiled.
realToFloating :: (Real a, Floating b) => a -> b
realToFloating = fromRational . toRational

