module Annotated where

import Data.Text (Text)
import Types (Expr)
import qualified Data.Text as T

data Annotation
  = Hyp Int Int
  | AxSch Int Int
  | Ax Int Int
  | MP Int Int Int
  | QuantorRule Text Int Int
  deriving stock (Eq, Ord)

data Annotated = Annotated Annotation Expr

instance Show Annotation where
  show = \case
    Hyp n k -> "[" <> show n <> ". Hyp. " <> show k <> "]"
    AxSch n k ->
      let number = if k == 13 then "A9" else show k in
      "[" <> show n <> ". Ax. sch. " <> number <> "]"
    Ax n k -> "[" <> show n <> ". Ax. A" <> show k <> "]"
    MP n k l -> "[" <> show n <> ". M.P. " <> show k <> ", " <> show l <> "]"
    QuantorRule q n k -> "[" <> show n <> ". " <> T.unpack q <> "-rule " <> show k <> "]"

instance Show Annotated where
  show (Annotated ann expr) = show ann <> " " <> show expr
