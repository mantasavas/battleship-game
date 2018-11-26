module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec

import BattleShipEngine
import Data

shipCoordsTest :: [Hit]
shipCoordsTest = [
      CoordHit {xAxis = 'F', yAxis = "9", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'G', yAxis = "9", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'H', yAxis = "9", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'I', yAxis = "9", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'A', yAxis = "1", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'B', yAxis = "1", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'B', yAxis = "2", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'C', yAxis = "2", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'D', yAxis = "4", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'E', yAxis = "4", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'D', yAxis = "5", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'E', yAxis = "5", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'A', yAxis = "10", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'B', yAxis = "10", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'C', yAxis = "10", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'B', yAxis = "9", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'A', yAxis = "6", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'B', yAxis = "6", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'C', yAxis = "6", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"},
      CoordHit {xAxis = 'C', yAxis = "7", shot = "CLEAN", oponentShot = "CLEAN", myShip = "EXIST"}
    ]

randomOponentHitsTestData :: [Hit]
randomOponentHitsTestData = [
      CoordHit {xAxis = 'A', yAxis = "1", shot = "CLEAN", oponentShot = "HIT", myShip = "NONE"},
      CoordHit {xAxis = 'D', yAxis = "5", shot = "CLEAN", oponentShot = "HIT", myShip = "NONE"},
      CoordHit {xAxis = 'A', yAxis = "7", shot = "CLEAN", oponentShot = "MIS", myShip = "NONE"}
    ]

-- Initiliazing (generating) coordinates for game
prepareCoords :: IO [Hit]
prepareCoords = do
  newBoard <- initiliazeGameBoard
  return (placeMyShips newBoard shipCoords)

markCoorinateHit :: Hit -> Hit
markCoorinateHit hit
  | ((myShip hit) == "EXIST") = CoordHit {xAxis = (xAxis hit), yAxis = (yAxis hit), shot = (shot hit), oponentShot = "HIT", myShip = "EXIST"} 
  | otherwise = hit

markAllCoordinatesHit :: [Hit] -> [Hit]
markAllCoordinatesHit [] = []
markAllCoordinatesHit (head:tail) = ((markCoorinateHit head):(markAllCoordinatesHit(tail)))


countNumberOfShips :: [Hit] -> Int
countNumberOfShips [] = 0
countNumberOfShips (head:tail)
  | (myShip head) == "EXIST" = countNumberOfShips(tail) + 1
  | otherwise = countNumberOfShips(tail) + 0

main :: IO ()
main = hspec $ do
  describe "BattleShipEngine.allShipsDestroyed" $ do
    context "when not all ships are destroyed" $ do
      it "returns false" $ do
        -- Generating coordinates for board
        -- board <- prepareCoords
        (allShipsDestroyed shipCoordsTest) `shouldBe` (False :: Bool)
    context "when all ships are destroyed" $ do
      it "returns true" $ do
        -- board <- prepareCoords
        let shipsHitted = markAllCoordinatesHit shipCoordsTest
        (allShipsDestroyed shipsHitted) `shouldBe` (True :: Bool)
  
  describe "BattleShipEngine.placeMyShips" $ do
    it "has equally 20 ships placed" $ do
      newBoard <- initiliazeGameBoard
      let placed = placeMyShips newBoard shipCoordsTest
      (countNumberOfShips placed) `shouldBe` (20 :: Int)

  describe "BattleShipEngine.checkIfHitMyShip" $ do
    context "when opponent misses my ship" $ do
      it "returns false" $ do
        placedShips <- prepareCoords
        (checkIfHitMyShip placedShips (randomOponentHitsTestData!!2)) `shouldBe` (False :: Bool)
    context "when opponent hits my ship" $ do
      it "returns true" $ do
        placedShips <- prepareCoords
        (checkIfHitMyShip placedShips (randomOponentHitsTestData!!0)) `shouldBe` (True :: Bool)


-- main :: IO ()
-- main = do
--   defaultMain (testGroup "Our Library Tests" [add5Test, unitTests])

-- -- sayYoTest :: TestTree
-- -- sayYoTest = testCase "Testing sayYo"
-- --   (assertEqual "Should say Yo to Friend!" "Yo Friend!" (sayYo "Friend"))



-- add5Test :: TestTree
-- add5Test = testCase "Testing add5"
--   (assertEqual "Should add 5 to get 10" 10 (add5 5))



-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT,
--     testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]


-- testCache01 :: Assertion
-- testCache01 =
--     trim (empty 3 :: Cache String Int) @?= empty 3

-- testCache02 :: Assertion
-- testCache02 = do
--     h  <- newHandle 10 :: IO (Handle String Int)
--     v1 <- cached h "foo" (return 123)
--     v1 @?= 123
--     v2 <- cached h "foo" (fail "should be cached")
--     v2 @?= 123