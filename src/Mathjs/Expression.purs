module Mathjs.Expression where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as T

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error, EXCEPTION)

import Mathjs.Matrix (MatrixF)
import Mathjs.Vector (VectorF)
import Mathjs.Util (MATHJS)

type Scope r = { | r }

type BigNumberF = { d :: Array Number, e :: Number, s :: Number }
type FractionF = {s :: Number, n :: Number, d :: Number}
type ComplexF = { re :: Number, im :: Number }

 -- // The expression parser supports booleans, numbers, bignumber, complex numbers, units, strings, matrices, and objects.
data Result =
  Boolean Boolean
  | Number Number
  | BigNumber BigNumberF
  | Fraction FractionF
  | Complex ComplexF
  -- | Unit UnitF
  | String String
  | Vector VectorF
  | Matrix MatrixF
  | Object (Array (Tuple String Result))
  | ResultSet (Array Result)
  | Exception Error
  | Undefined

instance showResult :: Show Result where
  show (Boolean a) = "(Boolean " <> show a <> ")"
  show (Number a) = "(Number " <> show a <> ")"
  show (BigNumber a) = "(BigNumber " <> show a.d <> " " <> show a.e <> " " <> show a.s <> ")"
  show (Fraction a) = "(Fraction " <> show a.s <> " " <> show a.n <> " " <> show a.d <> ")"
  show (Complex a) = "(Complex " <> show a.re <> " " <> show a.im <> ")"
  show (String a) = "(String " <> show a <> ")"
  show (Vector a) = "(Vector " <> show a._data <> " " <> show a._size <> ")"
  show (Matrix a) = "(Matrix " <> show a._data <> " " <> show a._size <> ")"
  show (Object a) = "(Object " <> show a <> ")"
  show (ResultSet a) = "(ResultSet " <> show a <> ")"
  show (Exception a) = "(Exception " <> show a <> ")"
  show Undefined = "(Undefined)"

instance eqResult :: Eq Result where
  eq (Boolean a) (Boolean b) = eq a b
  eq (Number a) (Number b) = eq a b
  eq (BigNumber a) (BigNumber b) = (eq a.d b.d) && (eq a.e b.e) && (eq a.s b.s)
  eq (Fraction a) (Fraction b) = (eq a.s b.s) && (eq a.n b.n) && (eq a.d b.d)
  eq (Complex a) (Complex b) = (eq a.re b.re) && (eq a.im b.im)
  eq (String a) (String b) = eq a b
  eq (Vector a) (Vector b) = (eq a._data b._data) && (eq a._size b._size)
  eq (Matrix a) (Matrix b) = (eq a._data b._data) && (eq a._size b._size)
  eq (Object a) (Object b) = eq a b
  eq (ResultSet a) (ResultSet b) = eq a b
  eq (Exception a) (Exception b) = false
  eq Undefined Undefined = true
  eq _ _ = false

isBoolean :: Result -> Boolean
isBoolean (Boolean _) = true
isBoolean _ = false

getBoolean :: Result -> Maybe Boolean
getBoolean (Boolean a) = pure a
getBoolean _ = Nothing

isNumber :: Result -> Boolean
isNumber (Number _) = true
isNumber _ = false

getNumber :: Result -> Maybe Number
getNumber (Number a) = pure a
getNumber _ = Nothing

isBigNumber :: Result -> Boolean
isBigNumber (BigNumber _) = true
isBigNumber _ = false

getBigNumber :: Result -> Maybe BigNumberF
getBigNumber (BigNumber a) = pure a
getBigNumber _ = Nothing

isFraction :: Result -> Boolean
isFraction (Fraction _) = true
isFraction _ = false

getFraction :: Result -> Maybe FractionF
getFraction (Fraction a) = pure a
getFraction _ = Nothing

isComplex :: Result -> Boolean
isComplex (Complex _) = true
isComplex _ = false

getComplex :: Result -> Maybe ComplexF
getComplex (Complex a) = pure a
getComplex _ = Nothing

isString :: Result -> Boolean
isString (String _) = true
isString _ = false

getString :: Result -> Maybe String
getString (String a) = pure a
getString _ = Nothing

isVector :: Result -> Boolean
isVector (Vector _) = true
isVector _ = false

getVector :: Result -> Maybe VectorF
getVector (Vector a) = pure a
getVector _ = Nothing

isMatrix :: Result -> Boolean
isMatrix (Matrix _) = true
isMatrix _ = false

getMatrix :: Result -> Maybe MatrixF
getMatrix (Matrix a) = pure a
getMatrix _ = Nothing

isObject :: Result -> Boolean
isObject (Object _) = true
isObject _ = false

isResultSet :: Result -> Boolean
isResultSet (ResultSet _) = true
isResultSet _ = false

isException :: Result -> Boolean
isException (Exception _) = true
isException _ = false

isUndefined :: Result -> Boolean
isUndefined Undefined = true
isUndefined _ = false


lookup :: Result -> String -> Maybe Result
lookup (Object a) str = T.lookup str a
lookup _ _ = Nothing

type ExpressionF = { eval :: ∀ r eff. (Scope r) -> Eff ( mathjs :: MATHJS, exception :: EXCEPTION | eff ) (Tuple Result (Scope r)) }
type Expression = ExpressionF

foreign import _compile ::
  ∀ eff.
  String ->
  Eff ( mathjs :: MATHJS, exception :: EXCEPTION | eff ) ExpressionF

type Constructors = {
  bool :: (Boolean -> Result),
  number :: (Number -> Result),
  bignumber :: (BigNumberF -> Result),
  fraction :: (FractionF -> Result),
  complex :: (ComplexF -> Result),
  string :: (String -> Result),
  vector :: (VectorF -> Result),
  matrix :: (MatrixF -> Result),
  pair :: (String -> Result -> Tuple String Result),
  obj :: (Array (Tuple String Result) -> Result),
  set :: (Array Result -> Result),
  undef :: (Result)
}

foreign import _eval ::
  ∀ r eff.
  (Result -> (Scope r) -> Tuple Result (Scope r)) ->
  Constructors ->
  ExpressionF ->
  (Scope r) ->
  Eff ( mathjs :: MATHJS, exception :: EXCEPTION | eff ) (Tuple Result (Scope r))

compile :: ∀ eff. String -> Eff ( mathjs :: MATHJS, exception :: EXCEPTION | eff) Expression
compile = _compile

eval :: ∀ r eff. Expression -> (Scope r) -> Eff ( mathjs :: MATHJS, exception :: EXCEPTION | eff ) (Tuple Result (Scope r))
eval = _eval Tuple { bool: Boolean, number: Number, bignumber: BigNumber, fraction: Fraction, complex: Complex, string: String, vector: Vector, matrix: Matrix, pair: Tuple, obj: Object, set: ResultSet, undef: Undefined }
