{-# LANGUAGE RecordWildCards #-}

module Construction.Internal.Types
  ( Name, Term(..)
  , Type (..), Context (..), Substitution (..)
  , Equation
  ) where

import           Data.Map  (Map (..), empty, insert, member, notMember, union, (!))
import           Data.Set  (Set (..))
import           Data.Text (Text, unpack)

type Name = Text -- just alias, no more

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M

instance Show Term where
  show Var{..} = unpack var
  show App{..} = "(" ++ showBr algo ++ " " ++ showBr arg ++ ")"
    where
      showBr :: Term -> String
      showBr v@Var{} = show v
      showBr t       = "(" ++ show t ++ ")"
  show Lam{..} = "\\" ++ unpack variable ++ "." ++ show body

data Type = TVar { tvar :: Name }                   -- Type variables: a, b, ...
          | TArr { from :: Type, to :: Type }       -- Arrow types: a -> b
  deriving (Ord)

instance Eq Type where
  t1 == t2 = (\(_, _, z) -> z) $ typeCompare empty empty t1 t2

typeCompare :: Map Name Name -> Map Name Name -> Type -> Type -> (Map Name Name, Map Name Name, Bool)
typeCompare m r (TVar v1) (TVar v2) | v1 `member` m && v2 `member` r = (m, r, m ! v1 == v2 && r ! v2 == v1)
                                    | v1 `notMember` m && v2 `notMember` r = (insert v1 v2 m, insert v2 v1 r, True)
                                    | otherwise = (empty, empty,  False)
typeCompare m r (TArr fr1 to1) (TArr fr2 to2) = let (m', r', eq1)   = typeCompare m r fr1 fr2
                                                    (m'', r'', eq2) = typeCompare m' r' to1 to2
                                              in (m'', r'', eq1 && eq2)
typeCompare _ _ _ _ = (empty, empty, False)

instance Show Type where
  show (TVar n)           = unpack n
  show (TArr (TVar n) to) = "(" ++ unpack n ++ " -> " ++ show to ++ ")"
  show (TArr from to)     = "((" ++ show from ++ ") -> " ++ show to ++ ")"

newtype Context = Context { getCtx :: Map Name Type } -- Types of variables
  deriving (Eq, Ord, Show)

newtype Substitution = Substitution { getSubs :: Map Name Type } -- Substitute type variable by some type
  deriving (Show)

type Equation = (Type, Type) -- Equation on types

instance Monoid Context where
  mempty = Context empty
  Context a `mappend` Context b = Context $ a `union` b

instance Monoid Substitution where
  mempty = Substitution empty
  Substitution a `mappend` Substitution b = Substitution $ a `union` b
