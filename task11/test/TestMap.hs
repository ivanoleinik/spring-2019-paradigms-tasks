{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Smoke tests" [
            testCase "empty" $
                let tr = empty :: m Int String in Map.null tr @?= True
            ,
            testCase "singleton" $
                let tr = singleton 4 "x" :: m Int String in do
                size tr         @?= 1
                member 4 tr     @?= True
                Map.lookup 4 tr @?= Just "x"
            ,
            testCase "fromList" $
                let tr = fromList [(4, "x"), (4, "y")] :: m Int String in do
                size tr         @?= 1
                member 4 tr     @?= True
                Map.lookup 4 tr @?= Just "y"
            ,
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(4, "x"), (1, "y"), (1, "z"), (5, "t")] :: m Int String in
                toAscList tr @?= [(1,"z"), (4,"x"), (5,"t")]
            ,
            testCase "insert" $
                let tr = insert 4 "x" empty :: m Int String in do
                size tr         @?= 1
                member 4 tr     @?= True
                Map.lookup 4 tr @?= Just "x"
            ,
            testCase "insertWith" $
                let tr = insertWith (++) 4 "y" (singleton 4 "x") :: m Int String in do
                size tr         @?= 1
                member 4 tr     @?= True
                Map.lookup 4 tr @?= Just "yx"
            ,
            testCase "insertWithKey" $
                let tr = insertWithKey (\k newV oldV -> show k ++ newV ++ oldV) 4 "y" (singleton 4 "x") :: m Int String in do
                size tr         @?= 1
                member 4 tr     @?= True
                Map.lookup 4 tr @?= Just "4yx"
            ,
            testCase "delete" $
                let tr = delete 4 (insert 4 "x" empty) :: m Int String in do
                Map.null tr     @?= True
                Map.lookup 4 tr @?= Nothing
            ,
            testCase "adjust" $
                let tr = adjust ("y" ++) 4 (singleton 4 "x") :: m Int String in
                Map.lookup 4 tr @?= Just "yx"
            ,
            testCase "adjustWithKey" $
                let tr = adjustWithKey (\k v -> show k ++ v) 4 (singleton 4 "x") :: m Int String in
                Map.lookup 4 tr @?= Just "4x"
            ,
            testCase "update" $
                let tr = update (const Nothing) 4 (singleton 4 "x") :: m Int String in
                Map.null tr @?= True
            ,
            testCase "updateWithKey" $
                let tr = updateWithKey (\k v -> Just $ show k ++ v) 4 (singleton 4 "x") :: m Int String in
                Map.lookup 4 tr @?= Just "4x"
            ,
            testCase "alter" $
                let tr = alter (fmap ("y" ++)) 4 (singleton 4 "x") :: m Int String in
                Map.lookup 4 tr @?= Just "yx"
            ,
            testCase "lookup" $
                let tr = singleton 4 "x" :: m Int String in
                Map.lookup 4 tr @?= Just "x"
            ,
            testCase "member" $
                let tr = singleton 4 "x" :: m Int String in
                member 4 tr @?= True
            ,
            testCase "notMember" $
                let tr = singleton 4 "x" :: m Int String in
                notMember 7 tr @?= True
            ,
            testCase "null" $
                let tr = singleton 4 "x" :: m Int String in
                Map.null tr @?= False
            ,
            testCase "size" $
                let tr = fromList [(4, "x"), (1, "y"), (1, "z"), (5, "t")] :: m Int String in
                size tr @?= 3
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
