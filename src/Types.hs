module Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

data Term
  = Term :+: Term
  | Term :*: Term
  | TNext Term
  | TVar Text
  | TZero
  deriving stock (Eq, Ord)

infixl :+:
infixl :*:

data Expr
  = Expr :->: Expr
  | Expr :|: Expr
  | Expr :&: Expr
  | ENeg Expr
  | EForall Text Expr
  | EExists Text Expr
  | Term :=: Term
  | EPred Text
  deriving stock (Eq, Ord)

infixl :|:
infixl :&:
infixr :->:

data Header = Header
  { hContext :: [Expr]
  , hToProve :: Expr
  }

instance Show Term where
  show = \case
    a :+: b -> "(" <> show a <> "+" <> show b <> ")"
    a :*: b -> "(" <> show a <> "*" <> show b <> ")"
    TNext a -> show a <> "'"
    TVar v -> T.unpack v
    TZero -> "0"

instance Show Expr where
  show = \case
    a :->: b -> "(" <> show a <> "->" <> show b <> ")"
    a :|: b -> "(" <> show a <> "|" <> show b <> ")"
    a :&: b -> "(" <> show a <> "&" <> show b <> ")"
    ENeg a -> "!(" <> show a <> ")"
    EForall v a -> "(@" <> T.unpack v <> "." <> show a <> ")"
    EExists v a -> "(?" <> T.unpack v <> "." <> show a <> ")"
    a :=: b -> "(" <> show a <> "=" <> show b <> ")"
    EPred v -> T.unpack v

instance Show Header where
  show Header{..} = intercalate "," (show <$> hContext) <> "|-" <> show hToProve
