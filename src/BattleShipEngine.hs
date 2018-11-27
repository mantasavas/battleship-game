{-# LANGUAGE OverloadedStrings #-}

module BattleShipEngine
    ( startSendingRequests,
      getRandomHit,
      generateRandomHit,
      resolveMaybe,
      generateFirstMove,
      markCoordinateHit,
      isPostRequest,
      placeMyShips,
      allShipsDestroyed,
      checkIfHitMyShip
    ) where

import qualified  Data.ByteString.Lazy.Char8 as L8
import HttpRequest
import System.Random
import Data
import Parser.Parser
import Control.Concurrent
import Parser.BattleShip
import Data.Aeson (decode)


startSendingRequests :: String -> String -> Int -> [Hit] -> MoveMsg -> IO()
startSendingRequests game player nmoves hitmoves prevMove = do
    putStrLn ("Your HIT: " ++ show (numberOfMyHits hitmoves) ++ ", Opponent HIT: " ++  show (numberOponentHits hitmoves) ++ ", NR: " ++ show nmoves ++ "...")
    -- threadDelay 2000000
    if (isPostRequest player nmoves) == True && nmoves == 0
        then do sendRequest game player prevMove
                startSendingRequests game player (nmoves + 1) hitmoves prevMove
    else if (isPostRequest player nmoves) == True && nmoves /= 0 && (checkEndingOfGame prevMove) == False
        then do rndCoord <- generateRandomHit hitmoves
                if (isSuccessful rndCoord) == True
                    then do body <- getRandomHit rndCoord prevMove hitmoves -- Decided whever to send oponent hit or miss (getRandomHit)
                            let modHitMoves = markRandomMove prevMove hitmoves  -- Mark for myself if oponent hited or missed my ship
                            if allShipsDestroyed modHitMoves == False
                                then do sendRequest game player (resolveMaybe body)
                                        startSendingRequests game player (nmoves + 1) modHitMoves (resolveMaybe body)
                            else if allShipsDestroyed modHitMoves == True
                                then do let lastMove = generateLastMove prevMove hitmoves
                                        sendRequest game player lastMove
                                        printStatistics modHitMoves False
                            else print "Something went wrong!"
                else if (isSuccessful rndCoord) == False 
                    then do sendRequest game player (generateLastMove prevMove hitmoves)
                            print "Unfortunately you run out of moves! This shouldn't happen!"
                else print "Something unpredictable happened!"
    
    else if (checkEndingOfGame prevMove) == False
        then do req <- receiveRequest game player
                let parsered = parseMessage (L8.unpack req)
                if (isSuccessfulEither parsered) == True 
                    then do let resultGet = getResultHit (resolveEither parsered)
                            if (resultGet == "HIT") || (resultGet == "MISS")
                                then do let movesHitConverted = convertToHitMovesPrev (resolveEither parsered)
                                        let hitmovesMod = markCoordinateHit hitmoves (movesHitConverted) resultGet
                                        startSendingRequests game player (nmoves + 1) hitmovesMod (resolveEither parsered)
                            else startSendingRequests game player (nmoves + 1) hitmoves (resolveEither parsered)                     
                else print "Failed parssing message! Maybe response from player took too long?"
    else if (checkEndingOfGame prevMove) == True
        then do printStatistics hitmoves True
    else print "Something went terribly wrong! Exiting..."


printStatistics :: [Hit] -> Bool -> IO()
printStatistics hits won = do
    print "======================================================="
    if won == True
       then do print "Congratulations, you won!"
    else print "Unfortunately, you lost!"
    print "Game Finished!, time for some statistics...."
    putStrLn ("Your hits are " ++ show (numberOfMyHits hits)) 
    putStrLn ("Opponent hits: " ++ show (numberOponentHits hits))
    print "======================================================="

isPostRequest :: String -> Int -> Bool
isPostRequest player nmove
    | ("A" == player) && (nmove `mod` 2 == 0) = True
    | ("A" == player) && (nmove `mod` 2 == 1) = False
    | ("B" == player) && (nmove `mod` 2 == 0) = False
    | ("B" == player) && (nmove `mod` 2 == 1) = True


markRandomMove :: MoveMsg -> [Hit] -> [Hit]
markRandomMove prevMove allMoves = 
    let
        convertedToMove = convertToHitMoves prevMove
    in
        if checkIfHitMyShip allMoves convertedToMove == True
            then do markMyCoordinateHit allMoves convertedToMove "HIT"
        else markMyCoordinateHit allMoves convertedToMove "MISS"


getRandomHit :: Maybe Hit -> MoveMsg -> [Hit] -> IO (Maybe MoveMsg)
getRandomHit hitMove prevMove allMoves = do
    if (isSuccessful hitMove) == False 
        then return Nothing
    -- TODO change to dinamic hit with check 
    else if checkIfHitMyShip allMoves (convertToHitMoves prevMove) == True
        then do return (Just (DicMsg[("coord",  CordList[CordValue [(xAxis (resolveMaybe hitMove))], CordValue (yAxis (resolveMaybe hitMove))]),("result", CordValue "HIT"), ("prev", prevMove)]))
    else return (Just (DicMsg[("coord",  CordList[CordValue [(xAxis (resolveMaybe hitMove))], CordValue (yAxis (resolveMaybe hitMove))]),("result", CordValue "MISS"), ("prev", prevMove)]))

generateLastMove :: MoveMsg -> [Hit] -> MoveMsg
-- TODO change to dinamic hit with check
generateLastMove prevMove allMoves
    |  checkIfHitMyShip allMoves (convertToHitMoves prevMove) == True = DicMsg[("coord",  CordList[]),("result", CordValue "HIT"), ("prev", prevMove)]
    |  otherwise = DicMsg[("coord",  CordList[]),("result", CordValue "MISS"), ("prev", prevMove)]

generateFirstMove :: Maybe Hit -> IO (MoveMsg)
generateFirstMove randomHit = do
    return (DicMsg[("coord",  CordList[CordValue [(xAxis (resolveMaybe randomHit))], CordValue (yAxis (resolveMaybe randomHit))]),("result", CordValue "null"), ("prev", CordValue "null")])


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


-- My shots as cordinates
-- States can be:
-- 1) HIT
-- 2) MISS
-- 3) CLEAN
markCoordinateHit :: [Hit] -> Hit -> String -> [Hit]
markCoordinateHit allCoords hitCoord typeHit = 
    let
        index = resolveMaybe(getIndexOfCoords allCoords hitCoord 0)
        removedCoords = removeItem hitCoord allCoords
        newCoord = CoordHit (xAxis hitCoord) (yAxis hitCoord) typeHit (oponentShot (allCoords!!index)) (myShip (allCoords!!index))
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
        index = resolveMaybe(getIndexOfCoords allCoords hitCoord 0)
        removedCoords = removeItem hitCoord allCoords
        newCoord = CoordHit (xAxis hitCoord) (yAxis hitCoord) (shot (allCoords!!index)) hitType (myShip (allCoords!!index))
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
        index = resolveMaybe(getIndexOfCoords allCoords hitCoord 0)
        removedCoords = removeItem hitCoord allCoords
        newCoord = CoordHit (xAxis hitCoord) (yAxis hitCoord) (shot (allCoords!!index)) (oponentShot (allCoords!!index)) shitExist
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

convertToHitMovesPrev :: MoveMsg -> Hit
convertToHitMovesPrev msg =
    let
        prev = getPreviosMove msg
        coordX = getCoordX prev
        coordY = getCoordY prev
    in
        CoordHit {xAxis = (convertStringToChar coordX), yAxis = coordY, shot = "CLEAN", oponentShot = "CLEAN", myShip = "NONE"}



-- removeString :: MoveMsg -> MoveMsg
-- removeString (sd, msg) = msg

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
    | (shot head) == "HIT" = (numberOfMyHits tail) + 1
    | (shot head) == "MISS" = (numberOfMyHits tail) + 0
    | (shot head) == "CLEAN" = (numberOfMyHits tail) + 0


checkIfHitMyShip :: [Hit] -> Hit -> Bool
checkIfHitMyShip [] opoShot = False
checkIfHitMyShip (head:tail) opoShot
    | ((xAxis head) == (xAxis opoShot)) && ((yAxis head) == (yAxis opoShot)) && ((myShip head) == "EXIST") = True
    | otherwise = (checkIfHitMyShip tail opoShot)


allShipsDestroyed :: [Hit] -> Bool
allShipsDestroyed hits
    | (numberOponentHits hits) - (length shipCoords) >= 0 = True
    | otherwise = False


convertToHitMoves :: MoveMsg -> Hit
convertToHitMoves msg =
    let
        coordX = getCoordX msg
        coordY = getCoordY msg
    in
        CoordHit {xAxis = (convertStringToChar coordX), yAxis = coordY, shot = "CLEAN", oponentShot = "CLEAN", myShip = "NONE"}
