module Axiom where

import Types (Expr(..), Term(..))
import Subst (checkSubst, checkFreeSubst, subst)
import Control.Monad (when, guard)
import Data.Text (Text)
import Parser (parseExpression)
import qualified Data.Map as M

getAxiomScheme :: Expr -> Maybe Int
getAxiomScheme = \case
  a :->: _ :->: a'
    | a == a' -> Just 1
  (a :->: b) :->: (a' :->: b' :->: c) :->: (a'' :->: c')
    | a == a' && a' == a'' && b == b' && c == c' -> Just 2
  a :->: b :->: (a' :&: b')
    | a == a' && b == b' -> Just 3
  (a :&: _) :->: a'
    | a == a' -> Just 4
  (_ :&: b) :->: b'
    | b == b' -> Just 5
  a :->: (a' :|: _)
    | a == a' -> Just 6
  b :->: (_ :|: b')
    | b == b' -> Just 7
  (a :->: c) :->: (b :->: c') :->: ((a' :|: b') :->: c'')
    | a == a' && b == b' && c == c' && c' == c'' -> Just 8
  (a :->: b) :->: (a' :->: ENeg b') :->: ENeg a''
    | a == a' && a' == a'' && b == b' -> Just 9
  ENeg (ENeg a) :->: a'
    | a == a' -> Just 10
  _ -> Nothing

getQuantorAxiom :: Bool -> Expr -> Maybe (Int, Text, Term)
getQuantorAxiom checkFree = \case
  a :->: (EExists x a') -> do
    t <- checkSubst x a' a
    when checkFree do
      guard $ checkFreeSubst x t a'
    pure (12, x, t)
  (EForall x a) :->: a' -> do
    t <- checkSubst x a a'
    when checkFree do
      guard $ checkFreeSubst x t a
    pure (11, x, t)
  _ -> Nothing

getFormalAxiom :: Expr -> Maybe Int
getFormalAxiom expr = axiomMap M.!? expr
  where
    axioms = parseExpression <$>
      [ "a = b -> a = c -> b = c"
      , "a = b -> a' = b'"
      , "a' = b' -> a = b"
      , "!(a' = 0)"
      , "a + 0 = a"
      , "a + b' = (a + b)'"
      , "a * 0 = 0"
      , "a * b' = a * b + a"
      ]

    axiomMap = M.fromList $ zip axioms [1..]

checkInduction :: Expr -> Bool
checkInduction = \case
  (a :&: (EForall x (a' :->: a''))) :->: a''' ->
    a == subst x TZero a' && a'' == subst x (TNext (TVar x)) a' && a''' == a'
  _ -> False
