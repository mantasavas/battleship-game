{-# LANGUAGE OverloadedStrings #-}

module HttpRequest
    ( sendRequest,
      receiveRequest
    ) where

import            Data.Aeson     (FromJSON, ToJSON, parseJSON, toJSON, (.:), withObject, pairs, toEncoding, decode, encode, object, (.=), Value(Null))
import qualified  Data.ByteString.Lazy.Char8 as L8
import            Network.HTTP.Client
import            Network.HTTP.Client.TLS
import            Network.HTTP.Types.Status  (statusCode)
import            Parser.Parser 
import            Data.Text
import            Parser.BattleShip

-- Lets override encode method
instance ToJSON MoveMsg where
  toJSON moveMsg = dataConverstionToObject moveMsg



-- Sending HTTP POST to server, for posting coordinates
sendRequest :: String -> String -> MoveMsg -> IO L8.ByteString
sendRequest game player body = do
  -- curl -H "Content-type: application/json" -X POST -d '{"coord":["B","5"],"result":null,"prev":null}' -v http://battleship.haskell.lt/game/test78/player/A
  -- curl -H "Accept: application/json" -X POST -v http://battleship.haskell.lt/game/test78/player/A
  
  manager <- newManager tlsManagerSettings

  initialRequest <- parseRequest ("http://battleship.haskell.lt/game/" ++ game ++ "/player/" ++ player)
  let request = initialRequest
          { 
            method = "POST",
            requestBody = RequestBodyLBS $ encode body,
            requestHeaders =
              [ 
                ("Content-Type", "application/json; charset=utf-8")
              ]
          }

  response <- httpLbs request manager
  -- putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  -- L8.putStrLn $ responseBody response
  return(responseBody response)


-- Sending HTTP GET to server, for getting coordinates
receiveRequest :: String -> String -> IO L8.ByteString
receiveRequest game player = do
  manager <- newManager tlsManagerSettings

  initialRequest <- parseRequest ("http://battleship.haskell.lt/game/" ++ game ++ "/player/" ++ player)
  let request = initialRequest
          { 
            method = "GET",
            requestHeaders =
              [ 
                ("Accept", "application/json; charset=utf-8")
              ]
          }

  response <- httpLbs request manager
  -- putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  -- L8.putStrLn $ responseBody response
  return(responseBody response)
