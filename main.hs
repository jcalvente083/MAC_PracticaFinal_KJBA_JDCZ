-- KEVIN JESÚS BANDA AZOGIL
-- JESÚS DAVID CALVENTE ZAPATA
module Main where
import Board
import Computer 
import Conecta4
import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)
import Control.Monad.RWS (MonadState(put))
import Data.Char (digitToInt)
import Data.List (intercalate)
import GHC.Exts (currentCallStack)
import Control.Concurrent (threadDelay)

-- | Convierte el tablero a un String para mostrarlo en la terminal.
imprimirTablero :: Tablero -> String
imprimirTablero b =
  intercalate "\n" (map (intercalate " | " . map showCelda) (transponer b)) ++ "\n" ++ replicate 29 '-'
  where
    showCelda 0 = " "
    showCelda 1 = "X"
    showCelda 2 = "O"

-- | Llama a la IA para jugar.
jugarIAConProfundidad :: Int -> Tablero -> Tablero
jugarIAConProfundidad = jugarIA

-- | Lee una columna del usuario y valida la entrada.
leerColumna :: IO Int
leerColumna = do
  putStr "Ingresa una columna (0-6): "
  entrada <- getLine
  case mapM (\c -> if c >= '0' && c <= '6' then Just (digitToInt c) else Nothing) entrada of
    Just [columna] -> return columna
    _              -> do
      putStrLn "Entrada inválida. Por favor, ingresa un número entre 0 y 6."
      leerColumna


-- | Bucle principal del juego.
loopJuego :: Tablero -> UTCTime -> IO ()
loopJuego tablero lastMod = do
  putStrLn "\nTablero actual:"
  putStrLn (imprimirTablero tablero)
  
  if haFinalizado tablero
    then
      if haGanado tablero
        then putStrLn "\nJuego terminado: ¡Ganaste!"
        else putStrLn "\nJuego terminado: ¡Empate!"
    else do
      putStrLn "Tu turno:"
      waitForFileChange lastMod
      tableroJugador <- readBoard

      putStrLn "\nTablero después de tu movimiento:"
      putStrLn (imprimirTablero tableroJugador)
      
      if haFinalizado tableroJugador
        then
          if haGanado tableroJugador
            then putStrLn "\nJuego terminado: ¡Ganaste!"
            else putStrLn "\nJuego terminado: ¡Empate!"
        else do
          putStrLn "Turno de la IA:"
          let tableroIA = jugarIAConProfundidad 4 tableroJugador
          putStrLn "\nTablero después del movimiento de la IA:"
          putStrLn (imprimirTablero tableroIA)
          writeBoard tableroIA 
          lastMod <- getModificationTime "board_state.txt"
          
          if haFinalizado tableroIA
            then
              if haGanado tableroIA
                then putStrLn "\nJuego terminado: ¡Perdiste!"
                else putStrLn "\nJuego terminado: ¡Empate!"
            else loopJuego tableroIA lastMod

-- | Función principal del programa.
main :: IO ()
main = do
  putStrLn "\n¡Bienvenido a Conecta 4 contra la IA!"
  putStrLn "Tú eres el jugador 1 (X), y la IA es el jugador 2 (O)."
  putStrLn "Elige una columna para insertar tu ficha."
  putStrLn "El tablero tiene columnas de 0 a 6."
  currentTime <- getModificationTime "board_state.txt"
  threadDelay (10000)
  loopJuego nuevoTablero currentTime
