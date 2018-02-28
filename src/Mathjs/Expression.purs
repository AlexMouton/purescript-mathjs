module Mathjs.Expression where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff, kind Effect)
import Mathjs.Matrix (MatrixF)
import Mathjs.Vector (VectorF)
import Mathjs.Util (MATHJS)

type Scope r = { | r }
type Error = String

 -- // The expression parser supports booleans, numbers, bignumber, complex numbers, units, strings, matrices, and objects.
data Result = Undefined
  | Boolean Boolean
  | Number Number
  | String String
  | Vector VectorF
  | Matrix MatrixF
  | Set (Array Result)

instance showResult :: Show Result where
  show Undefined = "Undefined"
  show (Boolean a)  = "(Boolean" <> show a <> ")"
  show (Number a)  = "(Number" <> show a <> ")"
  show (String a)  = "(String" <> show a <> ")"
  show (Vector a)  = "(Vector" <> show a._data <> " " <> show a._size <> ")"
  show (Matrix a)  = "(Matrix" <> show a._data <> " " <> show a._size <> ")"
  show (Set a)  = "(Set" <> show a <> ")"

instance eqResult :: Eq Result where
  eq Undefined Undefined = true
  eq (Boolean a) (Boolean b) = eq a b
  eq (Number a) (Number b) = eq a b
  eq (String a) (String b) = eq a b
  eq (Vector a) (Vector b) = (eq a._data b._data) && (eq a._size b._size)
  eq (Matrix a) (Matrix b) = (eq a._data b._data) && (eq a._size b._size)
  eq (Set a) (Set b) = eq a b
  eq _ _ = false

type ExpressionF = { eval :: ∀ r eff. (Scope r) -> Eff ( mathjs :: MATHJS | eff ) (Tuple Result (Scope r)) }
type Expression = ExpressionF

foreign import _compile ::
  ∀ eff.
  (Error -> Either Error ExpressionF) ->
  (ExpressionF -> Either Error ExpressionF) ->
  String ->
  Eff ( mathjs :: MATHJS | eff ) (Either Error ExpressionF)

foreign import _eval ::
  ∀ r eff.
  (Result -> (Scope r) -> Tuple Result (Scope r)) ->
  (Result) ->
  (Boolean -> Result) ->
  (Number -> Result) ->
  (String -> Result) ->
  (VectorF -> Result) ->
  (MatrixF -> Result) ->
  (Array Result -> Result) ->
  ExpressionF ->
  (Scope r) ->
  Eff ( mathjs :: MATHJS | eff ) (Tuple Result (Scope r))

compile :: ∀ eff. String -> Eff ( mathjs :: MATHJS | eff) (Either Error Expression)
compile = _compile Left Right

eval :: ∀ r eff. Expression -> (Scope r) -> Eff ( mathjs :: MATHJS | eff ) (Tuple Result (Scope r))
eval = _eval Tuple Undefined Boolean Number String Vector Matrix Set
