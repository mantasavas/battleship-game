module Main where

import BattleShipEngine
import Lib
import Data
import HttpRequest
import Parser.Parser
import qualified  Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do 
  -- someFunc
  -- sendRequest

  putStrLn "Enter player of your choice (A or B): "  
  player <- getLine
  putStrLn "Enter Game name: "
  game <- getLine
  -- initializing board for shots marking
  shots <- initiliazeGameBoard
  rndHit <- generateRandomHit shots
  body <- generateFirstMove rndHit
  let placedShips = placeMyShips shots shipCoords
  -- if (isPostRequest player 0) == True
  --   then do let shotsMarked = markCoordinateHit shots (resolveMaybe rndHit)
  --           startSendingRequests game player 0 shotsMarked body
  startSendingRequests game player 0 placedShips body

  putStrLn "Game has finished!"



