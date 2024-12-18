-- KEVIN JESÚS BANDA AZOGIL
-- JESÚS DAVID CALVENTE ZAPATA
module Board
  ( Tablero
  , nuevoTablero
  , poner
  , haFinalizado
  , transponer
  , getDiagonales
  , haGanado
  , lineaCompleta
  ) where

type Tablero = [[Int]]

nuevoTablero :: Tablero
nuevoTablero = replicate 7 (replicate 6 0)

addEnColumna :: Int -> [Int] -> [Int]
addEnColumna i [] = []  
addEnColumna i (x:xs)
  | x == 0    = i : xs  
  | otherwise = x : addEnColumna i xs 


poner :: Int -> Int -> Tablero -> Tablero
poner _ _ [] = []
poner i 0 (x:xs)
  | lineaCompleta x = x:xs
  | otherwise    = addEnColumna i x : xs
poner i c (x:xs) = x : poner i (c-1) xs

lineaCompleta :: [Int] -> Bool
lineaCompleta = all (/= 0)

tableroCompleto :: Tablero -> Bool
tableroCompleto = all lineaCompleta

transponer :: Tablero -> Tablero
transponer ([]:_) = []
transponer x      = map head x : transponer (map tail x)

getDiagonales :: Tablero -> [[Int]]
getDiagonales []      = []
getDiagonales ([]:xs) = xs
getDiagonales xs      = zipWith (++) (map ((:[]) . head) xs ++ repeat [])
                                  ([] : getDiagonales (map tail xs))

victoriaVertical :: Tablero -> Bool
victoriaVertical = any aux
  where
    aux (x1:x2:x3:x4:xs) = (x1 /= 0 && x1 == x2 && x2 == x3 && x3 == x4) || aux (x2:x3:x4:xs)
    aux _                = False

victoriaHorizontal :: Tablero -> Bool
victoriaHorizontal = victoriaVertical . transponer

victoriaDiagonal :: Tablero -> Bool
victoriaDiagonal = victoriaVertical . getDiagonales

victoriaDiagonal2 :: Tablero -> Bool
victoriaDiagonal2 = victoriaVertical . getDiagonales . reverse

haGanado :: Tablero -> Bool
haGanado b = victoriaVertical b || victoriaHorizontal b || victoriaDiagonal b || victoriaDiagonal2 b

haFinalizado :: Tablero -> Bool
haFinalizado b = haGanado b || tableroCompleto b
