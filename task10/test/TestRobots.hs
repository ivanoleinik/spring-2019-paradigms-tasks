import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        naruto = robot "Naruto" 8 10
        sasuke = robot "Sasuke" 7 7
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack naruto @?= 8

        , testCase "Test for getHealth" $
            getHealth sasuke @?= 7

        , testCase "Test for setName" $
            setName "Valtor" walter @?= robot "Valtor" 50 50

        , testCase "Test for setAttack" $
            setAttack 70 sasuke @?= robot "Sasuke" 70 7

        , testCase "Test for setHealth" $
            setHealth 100 naruto @?= robot "Nalter" 8 100

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for damage" $
            damage walter 5 @?= robot "Walter" 50 45

        , testCase "Test for isAlive" $ do
            isAlive walter @?= True

        , testCase "Test for fight" $
            fight naruto sasuke @?= naruto

        , testCase "Test for threeRoundFight" $
            threeRoundFight naruto walter @?= walter

        , testCase "Test for survivors" $
            survivors @?= [robot "Naruto" 8 10, robot "Sakura" 3 9, robot "Sasuke" 7 7]
        ]
