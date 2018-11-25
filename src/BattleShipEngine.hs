{-# LANGUAGE OverloadedStrings #-}

module BattleShipEngine
    ( startSendingRequests,
      getRandomHit,
      generateRandomHit,
      resolveMaybe,
      generateFirstMove,
      markCoordinateHit,
      isPostRequest
    ) where

import qualified  Data.ByteString.Lazy.Char8 as L8
import HttpRequest
import System.Random
import Data
import Parser.Parser
import Control.Concurrent
import Parser.BattleShip
import Data.Aeson (decode)

-- hiting :: Hit
-- hiting = CoordHit "A" "4" "HIT"

startSendingRequests :: String -> String -> Int -> [Hit] -> MoveMsg -> IO()
startSendingRequests game player nmoves hitmoves prevMove = do
    putStrLn ("Waiting for connection happen " ++ game ++ ", player: " ++ player ++ ", nr: " ++ show nmoves ++ "...")

    print "============================================================"
    print hitmoves
    print "============================================================"
    
    -- threadDelay 2000000
    if (isPostRequest player nmoves) == True && nmoves == 0
        then do sendRequest game player prevMove
                print "---- first move -----"
                startSendingRequests game player (nmoves + 1) hitmoves prevMove
    else if (isPostRequest player nmoves) == True && nmoves /= 0 && (checkEndingOfGame prevMove) == False
        then do rndCoord <- generateRandomHit hitmoves
                if (isSuccessful rndCoord) == True 
                    then do body <- getRandomHit rndCoord prevMove
                            sendRequest game player (resolveMaybe body)
                            print "----- nth move ------"
                            print body
                            let hitmovesMod = markCoordinateHit hitmoves (resolveMaybe rndCoord)
                            startSendingRequests game player (nmoves + 1) hitmovesMod (resolveMaybe body)
                else if (isSuccessful rndCoord) == False 
                    then do sendRequest game player (generateLastMove prevMove)
                            print "Unfortunately you run out of moves! This shouldn't happen!"
                else print "Something unpredictable happened!"
                     
    else if (checkEndingOfGame prevMove) == False
        then do print "----- waiting for any request.. -------"
                req <- receiveRequest game player
                let parsered = parseMessage (L8.unpack req)
                if (isSuccessfulEither parsered) == True 
                    then do print "successfuly parsed message!"
                            print parsered
                            startSendingRequests game player (nmoves + 1) hitmoves (resolveEither parsered)
                else print "Failed parssing message! Maybe response from player took too long?"
    else if (checkEndingOfGame prevMove) == True
        then do print "Congrats, you won the game! Found Game End Symbol (Empty Coordinates!)"
                print "Game statistics: 12:45"
    else print "Something went terribly wrong! Exiting..."
    
    -- L8.putStrLn shotsInf

isPostRequest :: String -> Int -> Bool
isPostRequest player nmove
    | ("A" == player) && (nmove `mod` 2 == 0) = True
    | ("A" == player) && (nmove `mod` 2 == 1) = False
    | ("B" == player) && (nmove `mod` 2 == 0) = False
    | ("B" == player) && (nmove `mod` 2 == 1) = True


getRandomHit :: Maybe Hit -> MoveMsg -> IO (Maybe MoveMsg)
getRandomHit hitMove prevMove = do
    if (isSuccessful hitMove) == False 
        then return Nothing
    else return (Just (DicMsg[("coord",  CordList[CordValue [(xAxis (resolveMaybe hitMove))], CordValue (yAxis (resolveMaybe hitMove))]),("result", CordValue "HIT"), ("prev", prevMove)]))

generateFirstMove :: Maybe Hit -> IO (MoveMsg)
generateFirstMove randomHit = do
    return (DicMsg[("coord",  CordList[CordValue [(xAxis (resolveMaybe randomHit))], CordValue (yAxis (resolveMaybe randomHit))]),("result", CordValue "null"), ("prev", CordValue "null")])

generateLastMove :: MoveMsg -> MoveMsg
generateLastMove prevMove = DicMsg[("coord",  CordList[]),("result", CordValue "HIT"), ("prev", prevMove)]

isSuccessful :: (Maybe a) -> Bool
isSuccessful (Nothing) = False
isSuccessful (Just a) = True

isSuccessfulEither :: (Either String a) -> Bool
isSuccessfulEither (Right hit) = True
isSuccessfulEither (Left msg) = False

-- Generate random hit in this sequence:
-- 1) Random shot, but if that place was already hit when
-- 2) Go from the beginning of coordinates list and find first free coordinate hit
-- 3) If 2 was unsuccessfull, that means all hits there made on board
generateRandomHit :: [Hit] -> IO (Maybe Hit)
generateRandomHit hitmoves = do
    rndInt <- randomRIO (0, (length hitmoves) - 1 :: Int)
    let followHit = findFirstFreeHit hitmoves
   
    let randomHit   |   checkMaybe followHit == False       = Nothing
                    |   shot (hitmoves!!rndInt) == "CLEAN"  = Just (hitmoves!!rndInt)
                    |   otherwise                           = followHit
    return(randomHit)


-- Because list cannot be modified (variable immutable), we need 
-- to remove and when again append modified value of coordinate
-- My shots as cordinates
-- States can be:
-- 1) HIT
-- 2) MISS
-- 3) CLEAN
markCoordinateHit :: [Hit] -> Hit -> [Hit]
markCoordinateHit allCoords hitCoord = 
    let 
        removedCoords = removeItem hitCoord allCoords
        newCoord = CoordHit (xAxis hitCoord) (yAxis hitCoord) "HIT" (oponentShot hitCoord) (myShip hitCoord)
    in
        newCoord:removedCoords 



-- Marks coordinate as openent hits it
-- States can be:
-- 1) HIT
-- 2) MISS
-- 3) CLEAN
markMyCoordinateHit :: [Hit] -> Hit -> String -> [Hit]
markMyCoordinateHit allCoords hitCoord hitType = 
    let 
        removedCoords = removeItem hitCoord allCoords
        newCoord = CoordHit (xAxis hitCoord) (yAxis hitCoord) (shot hitCoord) hitType (myShip hitCoord)
    in
        newCoord:removedCoords 


-- Because list cannot be modified (variable immutable), we need 
-- to remove and when again append modified value of coordinate
-- My ship coordinates
-- States can be:
-- 1) EXIST
-- 2) NONE
markMyShipCoordinate :: [Hit] -> Hit -> String -> [Hit]
markMyShipCoordinate allCoords hitCoord shitExist = 
    let 
        removedCoords = removeItem hitCoord allCoords
        newCoord = CoordHit (xAxis hitCoord) (yAxis hitCoord) (shot hitCoord) (oponentShot hitCoord) shitExist
    in
        newCoord:removedCoords 

-- Finds first coordinate that we can hit
-- There is no other represent Hit as nothing => nill so we use maybe
findFirstFreeHit :: [Hit] -> Maybe Hit
findFirstFreeHit [] = Nothing
findFirstFreeHit (tail:head)
    | shot tail == "CLEAN" = Just tail
    | otherwise            = findFirstFreeHit head


resolveMaybe :: Maybe a -> a
resolveMaybe (Just val) = val

resolveEither :: Either String a -> a
resolveEither (Right val) = val 

checkMaybe :: Maybe Hit -> Bool
checkMaybe (Just _)  = True
checkMaybe (Nothing) = False


-- Removes item from the list
removeItem :: Hit -> [Hit] -> [Hit]
removeItem _ []                 = []
removeItem x (y:ys) | compareCoord x y == True  = removeItem x ys
                    | otherwise                 = y : removeItem x ys


-- Finds coordinate and returns index in the list
getIndexOfCoords :: [Hit] -> Hit -> Int -> Maybe Int
getIndexOfCoords [] coord incr = Nothing
getIndexOfCoords (head:tail) coord incr
    | (compareCoord head coord) == True = Just incr
    | otherwise                         = getIndexOfCoords tail coord (incr + 1)


-- Compares two coordinates
compareCoord :: Hit -> Hit -> Bool
compareCoord firstArg secondArg
    | ((xAxis firstArg) == (xAxis secondArg)) && ((yAxis firstArg) == (yAxis secondArg)) = True
    | otherwise = False


-- First [Hit] = all coorindates
-- Second [Hit] = where I wanna locate coordinates
placeMyShips :: [Hit] -> [Hit] -> [Hit]
placeMyShips allCoord [] = allCoord
placeMyShips allCoord (head:tail) = placeMyShips (markMyShipCoordinate allCoord head "EXIST") tail

-- Functions for statistics
numberOponentHits :: [Hit] -> Int
numberOponentHits [] = 0
numberOponentHits (head:tail)
    | (oponentShot head) == "HIT" = (numberOponentHits tail) + 1
    | (oponentShot head) == "MISS" = (numberOponentHits tail) + 0
    | (oponentShot head) == "CLEAN" = (numberOponentHits tail) + 0

    
numberOfMyHits :: [Hit] -> Int
numberOfMyHits [] = 0
numberOfMyHits (head:tail)
    | (oponentShot head) == "HIT" = (numberOfMyHits tail) + 1
    | (oponentShot head) == "MISS" = (numberOfMyHits tail) + 0
    | (oponentShot head) == "CLEAN" = (numberOfMyHits tail) + 0
