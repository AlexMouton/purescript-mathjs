module Mathjs.Expression where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff, kind Effect)
import Mathjs.Matrix (MatrixF)
import Mathjs.Vector (VectorF)

foreign import data MATHJS :: Effect

type Scope r = { | r }
type Error = String

 -- // The expression parser supports booleans, numbers, bignumber, complex numbers, units, strings, matrices, and objects.
data ExpResult = ExpUndefined | ExpBoolean Boolean | ExpNumber Number | ExpString String | ExpVector VectorF | ExpMatrix MatrixF | ExpSet (Array ExpResult)

instance showExpResult :: Show ExpResult where
  show ExpUndefined = "ExpUndefined"
  show (ExpBoolean a)  = "(ExpBoolean" <> show a <> ")"
  show (ExpNumber a)  = "(ExpNumber" <> show a <> ")"
  show (ExpString a)  = "(ExpString" <> show a <> ")"
  show (ExpVector a)  = "(ExpVector" <> show a._data <> " " <> show a._size <> ")"
  show (ExpMatrix a)  = "(ExpMatrix" <> show a._data <> " " <> show a._size <> ")"
  show (ExpSet a)  = "(ExpSet" <> show a <> ")"

instance eqExpResult :: Eq ExpResult where
  eq ExpUndefined ExpUndefined = true
  eq (ExpBoolean a) (ExpBoolean b) = eq a b
  eq (ExpNumber a) (ExpNumber b) = eq a b
  eq (ExpString a) (ExpString b) = eq a b
  eq (ExpVector a) (ExpVector b) = (eq a._data b._data) && (eq a._size b._size)
  eq (ExpMatrix a) (ExpMatrix b) = (eq a._data b._data) && (eq a._size b._size)
  eq (ExpSet a) (ExpSet b) = eq a b
  eq _ _ = false

type ExpressionF = { eval :: ∀ r eff. (Scope r) -> Eff ( mathjs :: MATHJS | eff ) (Tuple ExpResult (Scope r)) }
type Expression = ExpressionF

foreign import _compile ::
  ∀ eff.
  (Error -> Either Error ExpressionF) ->
  (ExpressionF -> Either Error ExpressionF) ->
  String ->
  Eff ( mathjs :: MATHJS | eff ) (Either Error ExpressionF)

foreign import _eval ::
  ∀ r eff.
  (ExpResult -> (Scope r) -> Tuple ExpResult (Scope r)) ->
  (ExpResult) ->
  (Boolean -> ExpResult) ->
  (Number -> ExpResult) ->
  (String -> ExpResult) ->
  (VectorF -> ExpResult) ->
  (MatrixF -> ExpResult) ->
  (Array ExpResult -> ExpResult) ->
  ExpressionF ->
  (Scope r) ->
  Eff ( mathjs :: MATHJS | eff ) (Tuple ExpResult (Scope r))

compile :: ∀ eff. String -> Eff ( mathjs :: MATHJS | eff) (Either Error Expression)
compile = _compile Left Right

eval :: ∀ r eff. Expression -> (Scope r) -> Eff ( mathjs :: MATHJS | eff ) (Tuple ExpResult (Scope r))
eval = _eval Tuple ExpUndefined ExpBoolean ExpNumber ExpString ExpVector ExpMatrix ExpSet
