module Main where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Matrix (Matrix, (!), matrix, safeGet, setElem)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Prelude hiding (iterate)
import System.Random.TF (TFGen, mkTFGen)
import Utils (shuffle)

type Coords = (Int, Int)

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

neighbors :: Matrix a -> Coords -> [(a, Coords)]
neighbors m xy =
    [(a, (x, y)) | (x, y) <- allMoves xy, Just a <- [safeGet x y m]]

randMove :: TFGen -> QTable -> Coords -> ((Float, Coords), TFGen)
randMove seed qTable xy = (maximumBy (compare `on` fst) qTable', seed')
  where
    (qTable', seed') = shuffle (neighbors qTable xy) seed

updateMoveQ ::
       Float
    -> Float
    -> (TFGen -> QTable -> Coords -> ((Float, Coords), TFGen))
    -> RTable
    -> (QTable, Coords, TFGen)
    -> (QTable, Coords, TFGen)
updateMoveQ alpha gamma select rTable (qTable, xy, seed) =
    (setElem update xy qTable, snd future, seed')
  where
    (future, seed') = select seed qTable xy
    update =
        ((1 - alpha) * qTable ! xy) +
        (alpha * (rTable ! xy + (gamma * fst future)))

foldModel ::
       Coords
    -> Float
    -> Float
    -> RTable
    -> Int
    -> (QTable, Coords, TFGen)
    -> (QTable, TFGen, Int)
foldModel target alpha gamma rTable n model
    | xy == target = (qTable, seed, n)
    | otherwise = foldModel target alpha gamma rTable (n + 1) move
  where
    f = updateMoveQ alpha gamma randMove rTable
    move = f model
    (_, xy, _) = move
    (qTable, _, seed) = f move

lifetimes ::
       Coords
    -> Coords
    -> Float
    -> Float
    -> RTable
    -> QTable
    -> TFGen
    -> Int
    -> IO ()
lifetimes start target alpha gamma rTable qTable seed n
    | n <= 0 = f
    | otherwise =
        f >> lifetimes start target alpha gamma rTable qTable' seed' (n - 1)
  where
    f = print qTable' >> print n'
    (qTable', seed', n') =
        foldModel target alpha gamma rTable 0 (qTable, start, seed)

main :: IO ()
main =
    setLocaleEncoding utf8 >>
    lifetimes start target alpha gamma rTable qTable seed 50
  where
    start = (1, 1)
    target = (17, 3)
    n = 20
    m = 18
    alpha = 0.5
    gamma = 0.5
    seed = mkTFGen 0
    qTable = matrix n m (const 0) :: QTable
    rTable = initTable n m target (-0.1) 100 :: RTable
