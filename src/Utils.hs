module Utils where

import Data.Function (on)
import Data.List (partition)
import System.Random (next, random, split)
import System.Random.TF (TFGen)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = uncurry . on (,)

attach :: [a] -> TFGen -> [(a, Bool)] -> ([(a, Bool)], TFGen)
attach [] g accu = (accu, snd $ next g)
attach (x:xs) g accu = attach xs g' ((x, b) : accu)
  where
    (b, g') = random g

partitionR :: [a] -> TFGen -> (([a], TFGen), ([a], TFGen))
partitionR xs g = ((ls, lg), (rs, rg))
  where
    (ys, g') = attach xs g []
    (ls, rs) = mapPair (map fst) (partition snd ys)
    (lg, rg) = split g'

shuffle :: [a] -> TFGen -> ([a], TFGen)
shuffle [] g = ([], g)
shuffle [x] g = ([x], g)
shuffle xs g = (ls' ++ rs', rg')
  where
    ((ls', _), (rs', rg')) = mapPair (uncurry shuffle) (partitionR xs g)
