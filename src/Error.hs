module Error where

import Data.Text (Text)
import Types (Term)
import qualified Data.Text as T

data ProofError
  = OccursFree Int Text Text
  | IsNotFree Int Text Term Text
  | NotProved Int
  | BadProof
  deriving stock (Eq, Ord)

instance Show ProofError where
  show = \case
    OccursFree n v q -> "Expression " <> show n <> ": variable " <> T.unpack v <> " occurs free in " <> T.unpack q <> "-rule."
    IsNotFree n v t q -> "Expression " <> show n <> ": variable " <> T.unpack v <> " is not free for term " <> show t <> " in " <> T.unpack q <> "-axiom."
    NotProved n -> "Expression " <> show n <> " is not proved."
    BadProof -> "The proof proves different expression."
