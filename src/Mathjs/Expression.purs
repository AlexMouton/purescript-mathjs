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

-- Decimal
type BigNumberF = { d :: Array Number, e :: Number, s :: Number }
-- Complex
type ComplexF = { re :: Number, im :: Number }

 -- // The expression parser supports booleans, numbers, bignumber, complex numbers, units, strings, matrices, and objects.
data Result =
  Boolean Boolean
  | Number Number
  -- | BigNumber BigNumberF
  -- | Complex ComplexF
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

isNumber :: Result -> Boolean
isNumber (Number _) = true
isNumber _ = false

isString :: Result -> Boolean
isString (String _) = true
isString _ = false

isVector :: Result -> Boolean
isVector (Vector _) = true
isVector _ = false

isMatrix :: Result -> Boolean
isMatrix (Matrix _) = true
isMatrix _ = false

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

type ExpressionF = { eval :: ∀ r eff. (Scope r) -> Eff ( mathjs :: MATHJS, ex :: EXCEPTION | eff ) (Tuple Result (Scope r)) }
type Expression = ExpressionF

foreign import _compile ::
  ∀ eff.
  String ->
  Eff ( mathjs :: MATHJS, ex :: EXCEPTION | eff ) ExpressionF

foreign import _eval ::
  ∀ r eff.
  (Result -> (Scope r) -> Tuple Result (Scope r)) ->
  (Boolean -> Result) ->
  (Number -> Result) ->
  (String -> Result) ->
  (VectorF -> Result) ->
  (MatrixF -> Result) ->
  (String -> Result -> Tuple String Result) ->
  (Array (Tuple String Result) -> Result) ->
  (Array Result -> Result) ->
  (Result) ->
  ExpressionF ->
  (Scope r) ->
  Eff ( mathjs :: MATHJS, ex :: EXCEPTION | eff ) (Tuple Result (Scope r))

compile :: ∀ eff. String -> Eff ( mathjs :: MATHJS, ex :: EXCEPTION | eff) Expression
compile = _compile

eval :: ∀ r eff. Expression -> (Scope r) -> Eff ( mathjs :: MATHJS, ex :: EXCEPTION | eff ) (Tuple Result (Scope r))
eval = _eval Tuple Boolean Number String Vector Matrix Tuple Object ResultSet Undefined
