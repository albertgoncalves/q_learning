module Main where

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
    , (x', y') /= (x + 1, y + 1)
    ]

neighbors :: Matrix a -> Coords -> [a]
neighbors m xy = [a | (x, y) <- allMoves xy, Just a <- [safeGet x y m]]

updateQ :: Float -> Float -> RTable -> QTable -> Coords -> QTable
updateQ alpha gamma rs qs xy = setElem update xy qs
  where
    initialQ = qs ! xy
    initialR = rs ! xy
    future = maximum (neighbors rs xy)
    update = ((1 - alpha) * initialQ) + (alpha * (initialR + (gamma * future)))

main :: IO ()
main = print r >> (print . updateQ alpha gamma r q) start
  where
    n = 4
    m = 5
    start = (2, 1)
    target = (2, 2)
    r = initTable n m target (-0.1) 100 :: RTable
    q = (matrix n m . const) 0 :: QTable
    alpha = 0.5
    gamma = 0.5
