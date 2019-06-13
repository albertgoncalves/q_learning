module Main where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Matrix (Matrix, (!), matrix, safeGet, setElem)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Prelude hiding (iterate)
import System.Random.TF (TFGen, mkTFGen)
import Text.Printf (printf)
import Utils (shuffle)

type Coords = (Int, Int)

type Alpha = Float

type Gamma = Float

type QTable = Matrix Float

type RTable = Matrix Float

initTable :: Int -> Int -> Coords -> a -> a -> Matrix a
initTable i j xy a b = setElem b xy (blank a)
  where
    blank = matrix i j . const

allMoves :: Coords -> [Coords]
allMoves (x, y) =
    [ (x', y')
    | x' <- [x - 1 .. x + 1]
    , y' <- [y - 1 .. y + 1]
    , x' == x || y' == y
    , (x', y') /= (x, y)
    ]

validMoves :: Matrix a -> Coords -> [(a, Coords)]
validMoves m xy =
    [(a, (x, y)) | (x, y) <- allMoves xy, Just a <- [safeGet x y m]]

randomMove :: TFGen -> QTable -> Coords -> ((Float, Coords), TFGen)
randomMove seed qTable xy = (maximumBy (compare `on` fst) qTable', seed')
  where
    (qTable', seed') = shuffle (validMoves qTable xy) seed

updateMove ::
       Alpha
    -> Gamma
    -> RTable
    -> (QTable, Coords, TFGen)
    -> (QTable, Coords, TFGen)
updateMove alpha gamma rTable (qTable, xy, seed) =
    (setElem update xy qTable, snd future, seed')
  where
    (future, seed') = randomMove seed qTable xy
    update =
        ((1 - alpha) * qTable ! xy) +
        (alpha * (rTable ! xy + (gamma * fst future)))

foldModel ::
       Coords
    -> Alpha
    -> Gamma
    -> RTable
    -> Int
    -> (QTable, Coords, TFGen)
    -> (QTable, TFGen, Int)
foldModel target alpha gamma rTable n model
    | xy == target = (qTable, seed, n)
    | otherwise = foldModel target alpha gamma rTable (n + 1) move
  where
    f = updateMove alpha gamma rTable
    move = f model
    (_, xy, _) = move
    (qTable, _, seed) = f move

printResults :: QTable -> Int -> IO ()
printResults qTable n = print qTable >> (putStrLn . printf "%d\n") n

lifetimes ::
       Coords
    -> Alpha
    -> Gamma
    -> RTable
    -> [Coords]
    -> QTable
    -> TFGen
    -> IO ()
lifetimes target alpha gamma rTable = loop
  where
    f = foldModel target alpha gamma rTable 0
    loop [] _ _ = return ()
    loop [start] qTable seed = printResults qTable' n'
      where
        (qTable', _, n') = f (qTable, start, seed)
    loop (start:next) qTable seed =
        printResults qTable' n' >> loop next qTable' seed'
      where
        (qTable', seed', n') = f (qTable, start, seed)

main :: IO ()
main = setLocaleEncoding utf8 >> f
  where
    starts = replicate 5 (1, 1)
    target = (17, 3)
    n = 20
    m = 18
    alpha = 0.8
    gamma = 0.8
    seed = mkTFGen 0
    qTable = matrix n m (const 0)
    rTable = initTable n m target (-0.1) 100
    f = lifetimes target alpha gamma rTable starts qTable seed
