module Construction.Internal.Types
  ( Name, Term(..)
  , Type (..), Context (..), Substitution (..)
  , Equation
  ) where

import           Data.Map  (Map (..), empty, union)
import           Data.Set  (Set (..))
import           Data.Text (Text)


type Name = Text -- just alias, no more

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M
  deriving (Show) -- we deriving some common classes like Show.
                  -- With this deriving you can use function "show"
                  -- to print your term.

data Type = TVar { tvar :: Name }                   -- Type variables: a, b, ...
          | TArr { from :: Type, to :: Type }       -- Arrow types: a -> b
  deriving (Eq, Ord)

instance Show Type where
  show (TVar n)           = show n
  show (TArr (TVar n) to) = show n ++ " -> " ++ show to
  show (TArr from to)     = "(" ++ show from ++ ") -> " ++ show to

newtype Context = Context { getCtx :: Map Name Type } -- Types of variables
  deriving (Show)

newtype Substitution = Substitution { getSubs :: Map Name Type } -- Substitute type variable by some type
  deriving (Show)

type Equation = (Type, Type) -- Equation on types

instance Monoid Context where
  mempty = Context empty
  Context a `mappend` Context b = Context $ a `union` b

instance Monoid Substitution where
  mempty = Substitution empty
  Substitution a `mappend` Substitution b = Substitution $ a `union` b
