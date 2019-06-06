module Main where

import Data.List (maximumBy)
import Data.Matrix (Matrix, (!), matrix, safeGet, setElem)

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

updateQ :: Float -> Float -> RTable -> QTable -> Coords -> (QTable, Coords)
updateQ alpha gamma rs qs xy = (setElem update xy qs, snd future)
  where
    initialQ = qs ! xy
    initialR = rs ! xy
    future = maximumBy (\(a, _) (b, _) -> compare a b) (neighbors qs xy)
    update =
        ((1 - alpha) * initialQ) + (alpha * (initialR + (gamma * fst future)))

notYet :: Eq b => b -> (a, b) -> Bool
notYet p (_, q) = p /= q

main :: IO ()
main = (print . last . takeWhile (notYet target) . iterate f) (q, start)
  where
    n = 20
    m = 20
    start = (1, 1)
    target = (17, 16)
    r = initTable n m target (-0.1) 100 :: RTable
    q = (matrix n m . const) 0 :: QTable
    alpha = 0.5
    gamma = 0.5
    f = uncurry (updateQ alpha gamma r)
