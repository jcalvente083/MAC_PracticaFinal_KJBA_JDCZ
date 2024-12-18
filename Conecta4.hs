-- KEVIN JESÚS BANDA AZOGIL
-- JESÚS DAVID CALVENTE ZAPATA
module Conecta4 (
    filePath,
    readBoard,
    writeBoard,
    waitForFileChange
    ) where 

import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)
import Control.Concurrent (threadDelay)
import Data.List (transpose)
import Board
    
filePath :: FilePath
filePath = "board_state.txt"

createEmptyBoard :: IO ()
createEmptyBoard = writeBoard nuevoTablero

readBoard :: IO Tablero
readBoard = do
    contents <- readFile filePath
    let columns = map (map read . words) (lines contents)
    let board = transpose columns  -- Transponer para convertir columnas en filas
    return board

-- Escribir el tablero al archivo (transponiendo para guardar filas como columnas)
writeBoard :: Tablero -> IO ()
writeBoard board = do
    let columns = transpose board  -- Transponer para convertir filas en columnas
    let contents = unlines (map (unwords . map show) columns)
    writeFile filePath contents

waitForFileChange :: UTCTime -> IO ()
waitForFileChange lastModTime = do
    threadDelay 1000000
    newModTime <- getModificationTime filePath
    if newModTime /= lastModTime
        then return ()
        else waitForFileChange lastModTime
