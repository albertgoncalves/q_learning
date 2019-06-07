module Main where

import Data.Matrix (Matrix, (!), matrix, safeGet, setElem)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
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
randMove seed qs xy = (head qs', seed')
  where
    (qs', seed') = shuffle (neighbors qs xy) seed

updateQ ::
       Float
    -> Float
    -> (TFGen -> QTable -> Coords -> ((Float, Coords), TFGen))
    -> RTable
    -> (TFGen, QTable, Coords)
    -> (TFGen, QTable, Coords)
updateQ alpha gamma select rs (seed, qs, xy) =
    (seed', setElem update xy qs, snd future)
  where
    initialQ = qs ! xy
    initialR = rs ! xy
    (future, seed') = select seed qs xy
    update =
        ((1 - alpha) * initialQ) + (alpha * (initialR + (gamma * fst future)))

main :: IO ()
main =
    setLocaleEncoding utf8 >>
    mapM_ (print . (\(_, x, _) -> x)) (take lives $ iterate f start)
  where
    n = 20
    m = 7
    alpha = 0.5
    gamma = 0.5
    start = (mkTFGen 0, (matrix n m . const) 0 :: QTable, (1, 1))
    target = (17, 3)
    rs = initTable n m target (-0.1) 100 :: RTable
    lives = 10
    steps = 1000
    f = last . take steps . iterate (updateQ alpha gamma randMove rs)
