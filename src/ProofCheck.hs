module ProofCheck where

import Control.Lens
import Types (Expr ((:->:), EExists, EForall))
import Data.Map (Map)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader (ask))
import Axiom (getAxiomScheme, getQuantorAxiom, checkInduction, getFormalAxiom)
import qualified Data.Map as M
import Annotated (Annotated (Annotated), Annotation (..))
import Control.Monad.Extra (whenJust, unlessM)
import Control.Monad (when)
import Error (ProofError (OccursFree, IsNotFree, NotProved))
import Data.List (elemIndex)
import Subst (occursFree)

data ProofState = ProofState
  { psProved :: [Expr]
  , psModusPonens :: Map Expr [Expr]
  , psLine :: Int
  , psDone :: Bool
  , psAnnotated :: [Annotated]
  , psProofError :: Maybe ProofError
  }
makeLensesWith abbreviatedFields ''ProofState

type MonadProof m = (MonadState ProofState m, MonadReader (Map Expr Int) m)

mkAnnotation :: (MonadProof m) => Annotation -> Expr -> m ()
mkAnnotation ann expr = do
  annotated <>= [Annotated ann expr]
  line += 1
  done .= True

checkHypothesis :: (MonadProof m) => Expr -> m ()
checkHypothesis expr = unlessM (use done) do
  ctx <- ask
  curLine <- use line
  whenJust (ctx M.!? expr) \n -> do
    mkAnnotation (Hyp curLine n) expr

checkAxiomScheme :: forall m. (MonadProof m) => Expr -> m ()
checkAxiomScheme expr = unlessM (use done) do
  case getAxiomScheme expr of
    Just n -> annotateAxSch n
    Nothing -> do
      case getQuantorAxiom True expr of
        Just (n, _, _) -> annotateAxSch n
        Nothing -> do
          when (checkInduction expr) do
            annotateAxSch 13
  where
    annotateAxSch :: Int -> m ()
    annotateAxSch n = do
      curLine <- use line
      mkAnnotation (AxSch curLine n) expr

checkAxiom :: (MonadProof m) => Expr -> m ()
checkAxiom expr = unlessM (use done) do
  whenJust (getFormalAxiom expr) \n -> do
    curLine <- use line
    mkAnnotation (Ax curLine n) expr

checkModusPonens :: (MonadProof m) => Expr -> m ()
checkModusPonens expr = unlessM (use done) do
  mp <- use modusPonens
  whenJust (mp M.!? expr) \lst -> do
    provedExprs <- use proved
    whenJust (findFirst provedExprs lst) \ek -> do
      let el = ek :->: expr
      let km = elemIndex ek provedExprs
      let lm = elemIndex el provedExprs
      whenJust ((,) <$> km <*> lm) \(k, l) -> do
        curLine <- use line
        mkAnnotation (MP curLine (k + 1) (l + 1)) expr
  where
    findFirst :: [Expr] -> [Expr] -> Maybe Expr
    findFirst [] _ = Nothing
    findFirst (expr : xs) lst
      | expr `elem` lst = Just expr
      | otherwise = findFirst xs lst

checkExistsRule :: (MonadProof m) => Expr -> m ()
checkExistsRule expr = unlessM (use done) do
  case expr of
    (EExists x a) :->: b -> do
      provedExprs <- use proved
      let e = a :->: b
      when (e `elem` provedExprs && not (occursFree x b)) do
        whenJust (elemIndex e provedExprs) \k -> do
          curLine <- use line
          mkAnnotation (QuantorRule "?" curLine (k + 1)) expr
    _ -> pure ()

checkForallRule :: (MonadProof m) => Expr -> m ()
checkForallRule expr = unlessM (use done) do
  case expr of
    a :->: (EForall x b) -> do
      provedExprs <- use proved
      let e = a :->: b
      when (e `elem` provedExprs && not (occursFree x a)) do
        whenJust (elemIndex e provedExprs) \k -> do
          curLine <- use line
          mkAnnotation (QuantorRule "@" curLine (k + 1)) expr
    _ -> pure ()

mkError :: (MonadProof m) => ProofError -> m ()
mkError err = do
  proofError .= Just err
  done .= True

throwExistsRuleError :: (MonadProof m) => Expr -> m ()
throwExistsRuleError expr = unlessM (use done) do
  case expr of
    (EExists x a) :->: b -> do
      provedExprs <- use proved
      let e = a :->: b
      when (e `elem` provedExprs && occursFree x b) do
        curLine <- use line
        mkError (OccursFree curLine x "?")
    _ -> pure ()

throwForallRuleError :: (MonadProof m) => Expr -> m ()
throwForallRuleError expr = unlessM (use done) do
  case expr of
    a :->: (EForall x b) -> do
      provedExprs <- use proved
      let e = a :->: b
      when (e `elem` provedExprs && occursFree x a) do
        curLine <- use line
        mkError (OccursFree curLine x "@")
    _ -> pure ()

throwQuantorAxiomError :: (MonadProof m) => Expr -> m ()
throwQuantorAxiomError expr = unlessM (use done) do
  case getQuantorAxiom True expr of
    Just _ -> pure ()
    Nothing -> do
      whenJust (getQuantorAxiom False expr) \(n, x, t) -> do
        let quantor = if n == 1 then "@" else "?"
        curLine <- use line
        mkError (IsNotFree curLine x t quantor)

throwNotProvedError :: (MonadProof m) => m ()
throwNotProvedError = unlessM (use done) do
  curLine <- use line
  mkError (NotProved curLine)

checkLine :: (MonadProof m) => Expr -> m ()
checkLine expr = do
  err <- use proofError
  case err of
    Just _ -> pure ()
    Nothing -> do
      checkHypothesis expr
      checkAxiomScheme expr
      checkAxiom expr
      checkModusPonens expr
      checkExistsRule expr
      checkForallRule expr

      throwExistsRuleError expr
      throwForallRuleError expr
      throwQuantorAxiomError expr
      throwNotProvedError

      done .= False
      case expr of
        a :->: b -> modusPonens %= M.insertWith (<>) b [a]
        _ -> pure ()
      proved <>= [expr]
