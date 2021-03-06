module Main where

import BattleShipEngine
import Data
import HttpRequest
import Parser.Parser
import qualified  Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do 
  putStrLn "Enter player of your choice (A or B): "  
  player <- getLine
  putStrLn "Enter Game name: "
  game <- getLine

  -- initializing board for shots marking
  shots <- initiliazeGameBoard
  rndHit <- generateRandomHit shots
  body <- generateFirstMove rndHit
  let placedShips = placeMyShips shots shipCoords
  startSendingRequests game player 0 placedShips body



