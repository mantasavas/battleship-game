{-# LANGUAGE OverloadedStrings #-}

module Data ( cartesianProduct, Hit(..), initiliazeGameBoard, shipCoords, randomOponentHits, convertStringToChar ) where

-- First => X axis (A, B, C....)
-- Second => Y axis (1, 2, 3...)
-- Third => Hit or Miss
data Hit = CoordHit {
    xAxis :: Char,
    yAxis :: String,
    shot :: String,
    oponentShot :: String,
    myShip :: String
  } deriving (Show)

shipCoords :: [Hit]
shipCoords = [
      CoordHit {xAxis = 'A', yAxis = "1", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'D', yAxis = "5", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'C', yAxis = "8", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"}
    ]

randomOponentHits = [
      CoordHit {xAxis = 'A', yAxis = "1", shot = "CLEAN", oponentShot = "HIT", myShip = "NONE"},
      CoordHit {xAxis = 'D', yAxis = "5", shot = "CLEAN", oponentShot = "HIT", myShip = "NONE"},
      CoordHit {xAxis = 'A', yAxis = "7", shot = "CLEAN", oponentShot = "MIS", myShip = "NONE"}
    ]



convertStringToChar :: String -> Char
convertStringToChar str
  | str == "A" = 'A'
  | str == "B" = 'B'
  | str == "C" = 'C'
  | str == "D" = 'D'
  | str == "E" = 'E'
  | str == "F" = 'F'
  | str == "G" = 'G'
  | str == "H" = 'H'
  | str == "I" = 'I'
  | str == "J" = 'J'

data MoveMsg = DicMsg [(String, MoveMsg)]
              | CordList [MoveMsg] 
              | CordValue String  
  deriving Show

firstMove :: MoveMsg
firstMove = DicMsg [
    ("coord",  CordList[CordValue "A", CordValue "10"]),
    ("result", CordValue "null"),
    ("prev",   CordValue "null")
  ]

------------------- Lets generate all possible combinations of shots -----------------
initiliazeGameBoard :: IO [Hit]
initiliazeGameBoard = do
    let coords = cartesianProduct ['A'..'J'] ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
    return (convertDataToHit coords [])

cartesianProduct :: [Char] -> [String] -> [(Char, String)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

-- CLEAN => we haven't hit in this place
convertDataToHit :: [(Char, String)] -> [(Hit)] -> [(Hit)]
convertDataToHit [] hits = hits
convertDataToHit (head:tail) hits = convertDataToHit tail ((CoordHit (fst head) (snd head) "CLEAN" "CLEAN" "NONE"):hits)
