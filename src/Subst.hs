module Subst
  ( subst
  , checkSubst
  , checkFreeSubst
  , occursFree
  ) where

import Types (Expr(..), Term(..))
import Data.Text (Text)
import Data.List (find)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as S

split :: Text -> Expr -> Expr -> Maybe [(Term, Term)]
split v expr1 expr2 = case (expr1, expr2) of
  (a :->: b, c :->: d) -> do
    left <- split v a c
    right <- split v b d
    pure $ left <> right
  (a :&: b, c :&: d) -> do
    left <- split v a c
    right <- split v b d
    pure $ left <> right
  (a :|: b, c :|: d) -> do
    left <- split v a c
    right <- split v b d
    pure $ left <> right
  (ENeg a, ENeg b) -> split v a b
  (EForall x a, EForall y b)
    | v == x -> Just []
    | x == y -> split v a b
    | otherwise -> Nothing
  (EExists x a, EExists y b)
    | v == x -> Just []
    | x == y -> split v a b
    | otherwise -> Nothing
  (EPred a, EPred b)
    | a == b -> Just []
    | otherwise -> Nothing
  (a :=: b, c :=: d) -> Just [(a, c), (b, d)]
  _ -> Nothing

splitTerm :: Term -> Term -> Maybe [(Text, Term)]
splitTerm term1 term2 = case (term1, term2) of
  (a :+: b, c :+: d) -> do
    left <- splitTerm a c
    right <- splitTerm b d
    pure $ left <> right
  (a :*: b, c :*: d) -> do
    left <- splitTerm a c
    right <- splitTerm b d
    pure $ left <> right
  (TNext a, TNext b) -> splitTerm a b
  (TZero, TZero) -> Just []
  (TVar v, t) -> Just [(v, t)]
  _ -> Nothing

containsVar :: Text -> Term -> Bool
containsVar v = \case
  a :+: b -> containsVar v a || containsVar v b
  a :*: b -> containsVar v a || containsVar v b
  TNext a -> containsVar v a
  TVar v' -> v == v'
  TZero -> False

findSubstitution :: Text -> Expr -> Expr -> Maybe Term
findSubstitution v expr1 expr2 = do
  terms <- split v expr1 expr2
  (term1, term2) <- find (containsVar v . fst) terms

  substitutions <- splitTerm term1 term2
  snd <$> find ((== v) . fst) substitutions

substInTerm :: Text -> Term -> Term -> Term
substInTerm v t = \case
  a :+: b -> substInTerm v t a :+: substInTerm v t b
  a :*: b -> substInTerm v t a :*: substInTerm v t b
  TNext a -> TNext $ substInTerm v t a
  var@(TVar v')
    | v' == v -> t
    | otherwise -> var
  TZero -> TZero

subst :: Text -> Term -> Expr -> Expr
subst v t = \case
  a :->: b -> subst v t a :->: subst v t b
  a :|: b -> subst v t a :|: subst v t b
  a :&: b -> subst v t a :&: subst v t b
  ENeg a -> ENeg $ subst v t a
  ex@(EForall v' a)
    | v' == v -> ex
    | otherwise -> EForall v' $ subst v t a
  ex@(EExists v' a)
    | v' == v -> ex
    | otherwise -> EExists v' $ subst v t a
  a :=: b -> substInTerm v t a :=: substInTerm v t b
  pred@(EPred _) -> pred

checkSubst :: Text -> Expr -> Expr -> Maybe Term
checkSubst v expr1 expr2 = do
  t <- findSubstitution v expr1 expr2
  guard $ subst v t expr1 == expr2
  pure t

getVars :: Term -> Set Text
getVars = \case
  a :+: b -> getVars a `S.union` getVars b
  a :*: b -> getVars a `S.union` getVars b
  TNext a -> getVars a
  TVar v -> S.singleton v
  TZero -> S.empty

occursFree :: Text -> Expr -> Bool
occursFree v = \case
  a :->: b -> occursFree v a || occursFree v b
  a :|: b -> occursFree v a || occursFree v b
  a :&: b -> occursFree v a || occursFree v b
  ENeg a -> occursFree v a
  EForall v' a
    | v' == v -> False
    | otherwise -> occursFree v a
  EExists v' a
    | v' == v -> False
    | otherwise -> occursFree v a
  a :=: b -> containsVar v a || containsVar v b
  EPred _ -> False

checkFreeSubst :: Text -> Term -> Expr -> Bool
checkFreeSubst v t = go S.empty
  where
    go :: Set Text -> Expr -> Bool
    go bounded = \case
      a :->: b -> go bounded a && go bounded b
      a :|: b -> go bounded a && go bounded b
      a :&: b -> go bounded a && go bounded b
      ENeg a -> go bounded a
      EForall v' a
        | v' == v -> True
        | otherwise -> go (v' `S.insert` bounded) a
      EExists v' a
        | v' == v -> True
        | otherwise -> go (v' `S.insert` bounded) a
      a :=: b -> checkFreeSubstInTerm bounded a && checkFreeSubstInTerm bounded b
      EPred _ -> True

    vars = getVars t

    checkFreeSubstInTerm :: Set Text -> Term -> Bool
    checkFreeSubstInTerm bounded t'
      | containsVar v t' = null $ bounded `S.intersection` vars
      | otherwise = True
