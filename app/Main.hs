module Main where

import qualified Data.Text.IO as TIO
import Parser (parseHeader, parseExpression)
import Types (Header(..), Expr)
import qualified Data.Text as T
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Map (Map)
import Control.Monad.State (State, execState)
import ProofCheck (ProofState (..), checkLine)
import qualified Data.Map as M
import Control.Monad (forM_, when)
import Error (ProofError(BadProof))

type ProofMonad = ReaderT (Map Expr Int) (State ProofState)

main :: IO ()
main = do
  header@Header{..} <- parseHeader <$> TIO.getLine
  exprLines <- T.lines <$> TIO.getContents

  let expressions = parseExpression <$> exprLines
  let ctx = M.fromList $ zip hContext [1..]
  let initState = ProofState [] M.empty 1 False [] Nothing

  let act = forM_ @_ @ProofMonad expressions checkLine

  let ProofState{..} = execState (runReaderT act ctx) initState

  print header
  forM_ psAnnotated print
  case psProofError of
    Just err -> print err
    Nothing -> do
      when (last psProved /= hToProve) do
        print BadProof
