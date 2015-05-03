{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.CompiledExpression
import Data.Foldable
import qualified Data.Map as M
import Data.Traversable
import Test.Framework
import Test.Framework.Providers.QuickCheck2

ops2 :: [(String, (forall a. Floating a => a -> a -> a))]
ops2 = [ ("(+)", (+))
       , ("(-)", (-))
       , ("(*)", (*))
       , ("(/)", (/))
       , ("(**)", (**)) ]

ops1 :: [(String, (forall a. Floating a => a -> a))]
ops1 = [ ("abs", abs)
        ,("negate", negate)
        ,("signum", signum)
        ,("sqrt", sqrt)
        ,("exp", exp)
        ,("log", log)
        ,("sin", sin)
        ,("cos", cos)
        ,("tan", tan)
        ,("asin", asin)
        ,("acos", acos)
        ,("atan", atan)
        ,("sinh", sinh)
        ,("cosh", cosh)
        ,("tanh", tanh)]

main :: IO ()
main = defaultMain
  [ testGroup "basic operations"
    ((flip fmap ops1 $ \(name, op) -> testProperty name (testOp1 op)) ++
     (flip fmap ops2 $ \(name, op) -> testProperty name (testOp op)))
  , testGroup "operations on structures (Data.Map)"
    ((flip fmap ops2 $ \(name, op) -> testProperty name (testMapOp op)) ++
     (flip fmap ops1 $ \(name, op) -> testProperty name (testMapOp1 op)))
  ]

testMapOp1 :: (forall a. Floating a => a -> a)
          -> [(Int, Double)]
          -> Bool
testMapOp1 fun map' =
    let correct_result = fmap fun map
        compiled_expr = compileExpression traverse traverse
                                          (fmap fun)
                                          (fmap (const ()) map)
     in eq2T correct_result (compiled_expr map)
  where
    map = M.fromList map'

testMapOp :: (forall a. Floating a => a -> a -> a)
          -> [(Int, Double)]
          -> Double
          -> Bool
testMapOp fun map' val =
    let correct_result = fmap (fun val) map
        compiled_expr = compileExpression traverse traverse
                                          (fmap (fun (realToFloating val)))
                                          (fmap (const ()) map)
     in eq2T correct_result (compiled_expr map)
  where
    map = M.fromList map'

testOp1 :: (forall a. Floating a => a -> a)
        -> (Double -> Bool)
testOp1 op x =
    let correct_result = op x
        compiled_expr = compileExpression3 (\z -> op z)
     in correct_result `eq2` compiled_expr x

testOp :: (forall a. Floating a => a -> a -> a)
       -> (Double -> Double -> Bool)
testOp op x y =
    let correct_result = x `op` y
        compiled_expr = compileExpression3 (\z -> z `op` realToFloating y)
        compiled_expr2 = compileExpression3 (\z -> realToFloating x `op` z)
     in correct_result `eq2` compiled_expr x &&
        correct_result `eq2` compiled_expr2 y

eq2T :: (Traversable f, Traversable f2)
     => f Double -> f2 Double -> Bool
eq2T cont1 cont2 =
    let l = toList cont1
        l2 = toList cont2
        l3 = zip l l2
     in all (\(x, y) -> eq2 x y) l3

eq2 :: Double -> Double -> Bool
eq2 x y
   | isNan x && isNan y = True
   | isNan x = False
   | isNan y = False
   | otherwise = x == y

isNan :: Double -> Bool
isNan x = x /= x

