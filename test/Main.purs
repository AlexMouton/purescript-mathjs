module Test.Main where

import Prelude (Unit, bind, discard, join, map, negate, ($), (<$>), (<*>), (==), (<<<), show)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..), fromRight, isRight, isLeft)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (sequence)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (try, EXCEPTION)
import Control.Monad.Eff.AVar (AVAR)

import Partial.Unsafe (unsafePartial)

import Test.Unit (test, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

-- import Mathjs.Util
import Mathjs.Geometry (Line(..), Point2D(..), distance, intersect)
import Mathjs.Matrix (MatrixError(..), det, diag, dot, eye, eye', flatten, getData, getSizes, inv, make, ones, ones', trace, transpose, zeros, zeros')
import Mathjs.Expression as Exp
import Mathjs.Util (MATHJS)

expresult str r = do
  let scope = { haha: "Haha" }
  cmp <- liftEff $ Exp.compile str
  ev <- liftEff $ Exp.eval cmp scope
  Assert.assert "Evaluates to Boolean" $ r == (fst ev)

main :: forall eff. Eff ( console :: CONSOLE , testOutput :: TESTOUTPUT , avar :: AVAR, mathjs :: MATHJS, exception :: EXCEPTION | eff ) Unit
main = runTest do
    suite "Geometry" do

        test "distance" do
            let p1 = Point2D 0.0 0.0
            let p2 = Point2D 4.0 3.0
            let p3 = Point2D 0.0 0.0
            Assert.assert "should be 5.0" $ distance p1 p2 == 5.0
            Assert.assert "should be 0.0" $ distance p3 p1 == 0.0

        test "intersection" do
            let l1 = Line (Point2D 3.0 6.0) (Point2D 4.0 7.0)
            let l2 = Line (Point2D 2.0 8.0) (Point2D 10.0 8.0)
            let l3 = Line (Point2D 2.0 8.0) (Point2D 10.0 8.0) -- same as above
            Assert.assert "should be intersect at 5.0 8.0" $ (intersect l1 l2) == Just (Point2D 5.0 8.0)
            Assert.assert "should not intersect" $ (intersect l2 l2) == Nothing

    suite "Expression" do
      suite "Compilation" do
        test "success" do
          let str = "x = 3"
          cmp <- liftEff $ try $ Exp.compile str
          Assert.assert "Compiles good" $ isRight cmp

        test "failure" do
          let str = "x ="
          cmp <- liftEff $ try $ Exp.compile str
          Assert.assert "Compiles bad" $ isLeft cmp

      suite "Eval" do
        test "eval boolean" $ expresult "x = false" (Exp.Boolean false)
        test "eval number" $ expresult "x = 3" (Exp.Number 3.0)
        test "eval bignumber" $ expresult "x = bignumber(100)" (Exp.BigNumber {s: 1.0, e: 2.0, d: [100.0]})
        test "eval fraction" $ expresult "x = fraction(100, 200)" (Exp.Fraction {s: 1.0, n: 1.0, d: 2.0})
        test "eval complex" $ expresult "x = 4 - 2i" (Exp.Complex {re: 4.0, im: -2.0})
        test "eval string" $ expresult "x = \"hehe\"" (Exp.String "hehe")
        test "eval vector" $ expresult "x = [1,2,3,4]" (Exp.Vector {_data: [1.0, 2.0, 3.0, 4.0], _size: [4]} )
        test "eval matrix" $ expresult "x = [[1,2],[3,4]]" (Exp.Matrix {_data: [[1.0, 2.0], [3.0, 4.0]], _size: [2,2]})
        test "eval object" $ expresult "x = {x: \"haha\", y: 3}" (Exp.Object [Tuple "x" (Exp.String "haha"), Tuple "y" (Exp.Number 3.0)])

        test "eval exception" do
          let str = "x = 3/\"haha\""
          let scope = { haha: "Haha" }
          cmp <- liftEff $ Exp.compile str
          ev <- liftEff $ try $ Exp.eval cmp scope
          Assert.assert "Evaluates to Exception" $ isLeft ev

        test "set scope" do
          let str = "haha = \"nono\""
          let scope = { haha: "Haha" }
          cmp <- liftEff $ Exp.compile str
          ev <- liftEff $ Exp.eval cmp scope
          Assert.assert "Evaluates to String" $ (Exp.String "nono") == (fst ev)
          Assert.assert "assigns nono" $ "nono" == ( _.haha $ snd ev)


    suite "Matrix" do

        test "make success" do
            let m = make [[1.0, 2.0], [3.0, 4.0]]
            let d = getData <$> m
            let s = getSizes <$> m
            Assert.assert "data should be be [[1.0, 2.0], [3.0, 4.0]]" $ d == Right [[1.0, 2.0], [3.0, 4.0]]
            Assert.assert "size should be be [2, 2]" $  s == Right (Tuple 2 2)

        test "make fails" do
            let m = make [[1.0, 2.0], [3.0]]
            Assert.assert "should be an InvalidSize " $ m == Left (InvalidRowSize 1)

        test "eye" do
            let e1 = getData $ eye 3 3
            let e2 = getData $ eye' 3
            let e3 = getData $ eye 2 3
            let r1 = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
            let r2 = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]]
            Assert.assert "should be [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]" $ e1 == r1
            Assert.assert "should be [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]" $ e2 == r1
            Assert.assert "should be [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]]" $ e3 == r2

        test "zeros" do
            let e1 = getData $ zeros 2 2
            let e2 = getData $ zeros' 2
            let e3 = getData $ zeros 1 3
            let r1 = [[0.0, 0.0], [0.0, 0.0]]
            let r2 = [[0.0, 0.0, 0.0]]
            Assert.assert "should be [[0.0, 0.0], [0.0, 0.0]]" $ e1 == r1
            Assert.assert "should be [[0.0, 0.0], [0.0, 0.0]]" $ e2 == r1
            Assert.assert "should be [[0.0, 0.0, 0.0]]" $ e3 == r2

        test "ones" do
            let e1 = getData $ ones 2 2
            let e2 = getData $ ones' 2
            let e3 = getData $ ones 1 3
            let r1 = [[1.0, 1.0], [1.0, 1.0]]
            let r2 = [[1.0, 1.0, 1.0]]
            Assert.assert "should be [[1.0, 1.0], [1.0, 1.0]]" $ e1 == r1
            Assert.assert "should be [[1.0, 1.0], [1.0, 1.0]]" $ e2 == r1
            Assert.assert "should be [[1.0, 1.0, 1.0]]" $ e3 == r2

        suite "dot" do
            test "has InvalidVectorSize" do
                let m1 = make [[1.0, 2.0, 3.0, 4.0]]
                let m2 = make [[1.0, 2.0, 3.0]]
                let r = join $ dot <$> m1 <*> m2
                Assert.assert "should be InvalidVectorSize" $ r == (Left $ InvalidVectorSize 4 3)

            test "has VectorsExpected" do
                let m1 = make [[1.0, 2.0, 3.0, 4.0],[1.0, 2.0, 3.0, 4.0]]
                let m2 = make [[1.0, 2.0, 3.0, 4.0],[1.0, 2.0, 3.0, 4.0]]
                let r = join $ dot <$> m1 <*> m2
                Assert.assert "should be VectorsExpected" $ r == (Left VectorsExpected)

            test "success" do
                let m1 = make [[1.0, 2.0, 3.0, 4.0]]
                let m2 = make [[1.0, 2.0, 3.0, 4.0]]
                let r = join $ dot <$> m1 <*> m2
                Assert.assert "should be VectorsExpected" $ r == (Right 30.0)

        suite "det" do
            test "is not a square matrix" do
                let m1 = make [[1.0, 2.0, 3.0], [1.0, 2.0, 3.0]]
                let r = join $ det <$> m1
                Assert.assert "should be SquareMatrixExpected" $ r == (Left SquareMatrixExpected)

            test "success" do
                let m1 = make [[-2.0, 2.0, 3.0], [-1.0, 1.0, 3.0], [2.0, 0.0, -1.0]]
                let r = join $ det <$> m1
                Assert.assert "should be success" $ r == (Right 6.0)

        suite "inv" do

            test "is not a square matrix" do
                let m1 = make [[1.0, 2.0, 3.0], [1.0, 2.0, 3.0]]
                let r = join $ inv <$> m1
                Assert.assert "should be SquareMatrixExpected" $ r == (Left SquareMatrixExpected)

            test "success" do
                let m1 = make [[1.0, 2.0], [3.0, 4.0]]
                let r = join $ inv <$> m1
                let d = getData <$> r
                Assert.assert "should be success" $ d == (Right  [[-2.0, 1.0], [1.5, -0.5]])

        suite "transpose" do

            test "success" do
                let m1 = make [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
                let r = getData <$> transpose <$> m1
                Assert.assert "should be success" $ r == (Right [[1.0, 4.0], [2.0, 5.0], [3.0, 6.0]])

        suite "flatten" do

            test "success" do
                let m1 = make [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
                let r = getData <$> flatten <$> m1
                Assert.assert "should be success" $ r == (Right [[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]])

        suite "trace" do

            test "is not a square matrix" do
                let m1 = make [[1.0, 2.0, 3.0], [1.0, 2.0, 3.0]]
                let r = join $ trace <$> m1
                Assert.assert "should be SquareMatrixExpected" $ r == (Left SquareMatrixExpected)

            test "success" do
                let m1 = make [[1.0, 2.0, 3.0], [-1.0, 2.0, 3.0], [2.0, 0.0, 3.0]]
                let r = join $ trace <$> m1
                Assert.assert "should be success" $ r == (Right 6.0)

        suite "diag" do

            test "is not a square matrix" do
                let m1 = make [[1.0, 2.0, 3.0], [1.0, 2.0, 3.0]]
                let r = join $ diag <$> m1
                Assert.assert "should be SquareMatrixExpected" $ r == (Left SquareMatrixExpected)

            test "success" do
                let m1 = make [[1.0, 2.0, 3.0], [-1.0, 2.0, 3.0], [2.0, 0.0, 3.0]]
                let r = join $ diag <$> m1
                let d = getData <$> r
                Assert.assert "should be success" $ d == (Right [[1.0, 2.0, 3.0]])
