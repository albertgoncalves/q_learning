module Utils where

import Data.Function (on)
import Data.List (partition)
import System.Random (next, random, split)
import System.Random.TF (TFGen)

iterateFor :: Int -> (a -> a) -> a -> a
iterateFor 0 f x = f x
iterateFor n f x = iterateFor (n - 1) f (f x)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = uncurry . on (,)

attachBool :: [a] -> TFGen -> [(a, Bool)] -> ([(a, Bool)], TFGen)
attachBool [] g accu = (accu, snd $ next g)
attachBool (x:xs) g accu = attachBool xs g' ((x, b) : accu)
  where
    (b, g') = random g

randPartition :: [a] -> TFGen -> (([a], TFGen), ([a], TFGen))
randPartition xs g = ((ls, lg), (rs, rg))
  where
    (ys, g') = attachBool xs g []
    (ls, rs) = mapPair (map fst) (partition snd ys)
    (lg, rg) = split g'

shuffle :: [a] -> TFGen -> ([a], TFGen)
shuffle [] g = ([], g)
shuffle [x] g = ([x], g)
shuffle xs g = (ls' ++ rs', rg')
  where
    ((ls', _), (rs', rg')) = mapPair (uncurry shuffle) (randPartition xs g)
