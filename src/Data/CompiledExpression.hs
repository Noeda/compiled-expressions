{-# LANGUAGE RankNTypes #-}

module Data.CompiledExpression
    ( -- * Core functionality
      compileExpression
      -- * Utilities
      --
      -- This functions are implemented in terms of `compileExpression`
    , compileExpression1
    , compileExpression2
    , compileExpression3 )
    where

import Data.CompiledExpression.Internal
import Data.Functor.Identity

-- | Same as `compileExpression` but does not use a container in output.
compileExpression1 :: Traversable f
                   => (forall a. Floating a => f a -> a)
                   -> f b
                   -> (f Double -> Double)
compileExpression1 fun source =
    let result_fun = compileExpression (\input -> Identity (fun input)) source
     in \input -> runIdentity (result_fun input)

-- | Same as `compileExpression` but does not use containers at all, just input
-- and output.
compileExpression2 :: (forall a. Floating a => a -> a)
                   -> (Double -> Double)
compileExpression2 fun =
    let result_fun = compileExpression (\(Identity input) -> Identity (fun input))
                                       (Identity ())
     in \input -> runIdentity (result_fun (Identity input))

-- | Same as `compileExpression` but does not use a container in input.
compileExpression3 :: Traversable f
                   => (forall a. Floating a => a -> f a)
                   -> (Double -> f Double)
compileExpression3 fun =
    let result_fun = compileExpression (\(Identity input) -> fun input)
                                       (Identity ())
     in \input -> result_fun (Identity input)

