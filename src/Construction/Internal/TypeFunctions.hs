{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Construction.Internal.TypeFunctions where

import           Construction.Internal.Functions hiding (Context, substitute)
import           Construction.Internal.Parser    (greedy, termP, typeP)
import           Construction.Internal.Types
import           Control.Arrow                   ((***))
import           Data.Either                     (isLeft, isRight)
import           Data.Map                        (Map (..), fromList, member,
                                                  union)
import qualified Data.Map                        as M (insert, singleton,
                                                       toList, (!))
import           Data.Set                        (Set (..), delete, elemAt,
                                                  insert, singleton, toList,
                                                  unions)
import qualified Data.Set                        as S
import           Data.String                     (IsString (..))
import           Data.Text                       (pack)
import           Text.Parsec                     (parse)

-- Split a set of elements to the first element and rest set
split :: Ord a => Set a -> (a, Set a)
split set = let x = elemAt 0 set
            in  (x, delete x set)

-- Take variable type from context or return Nothing
(!) :: Context -> Name -> Maybe Type
ctx ! x | member x (getCtx ctx) = Just $ getCtx ctx M.! x
        | otherwise             = Nothing

instance IsString Term where
  fromString s = case result of
                    Left msg  -> error $ show msg
                    Right res -> res
    where
      result = parse (greedy termP) "Term parser" . pack $ s


instance IsString Type where
  fromString s = case result of
                    Left msg  -> error $ show msg
                    Right res -> res
    where
      result = parse (greedy typeP) "Type parser" . pack $ s

-- Something we can perform substitution with
class Substitutable a where
  substitute :: Substitution -> a -> a

-- Substitution in context
--   [a:=t]empty       => empty
--   [a:=t]{x:t1 ... } => {x:([a:=t]t1) ... }
instance Substitutable Context where
  substitute sub (Context ctx) = Context $ fmap (substitute sub) ctx

-- Substitution in type:
--   [a:=t] a     => t
--   [a:=t] b     => b
--   [a:=t](r->p) => ([a:=t]r)->([a:=t]p)
instance Substitutable Type where
  substitute (Substitution sub) var@(TVar name) | name `member` sub = sub M.! name
                                                | otherwise = var
  substitute s@(Substitution sub) (TArr from to) = TArr (substitute s from) (substitute s to)

-- Compose two substitutions
-- S@[a1 := t1, ...] . [b1 := s1 ...] = [b1 := S(s1) ... a1 := t1 ...]
compose :: Substitution -> Substitution -> Substitution
compose bc ab = Substitution $ fmap (substitute bc) (getSubs ab) `union` getSubs bc

-- Create new context from free variables of some term
contextFromTerm :: Term -> Context
contextFromTerm term = Context $ fromList $ zip (toList $ free term) vars
  where
    vars = fmap (TVar . pack . ('a':) . show) [1..]

-- Find a substitution that can solve the set of equations
u :: [Equation] -> Maybe Substitution
u []       = pure mempty
u (x:rest) = singletonU x rest

singletonU :: Equation -> [Equation] -> Maybe Substitution
singletonU (sig, tau) rest | isVar sig && isVar tau && tvar sig == tvar tau = u rest
                           | isVar sig && (tau `contains` tvar sig) = Nothing
                           | isVar sig = let sub = Substitution $ M.singleton (tvar sig) tau
                                             rest' = (substitute sub *** substitute sub) <$> rest
                                         in compose <$> u rest' <*> pure sub
                           | isVar tau = singletonU (tau, sig) rest
                           | otherwise = let (TArr sig1 sig2) = sig
                                             (TArr tau1 tau2) = tau
                                         in u ((sig1, tau1) : (sig2, tau2) : rest)

isVar :: Type -> Bool
isVar (TVar _) = True
isVar _        = False

contains :: Type -> Name -> Bool
contains (TVar n) ref       = n == ref
contains (TArr sig tau) ref = (sig `contains` ref) || (tau `contains` ref)

getCtxNames :: Map Name Type -> Set Name
getCtxNames = S.unions . fmap (nameSet . snd) . M.toList

updateContext :: Context -> Name -> Type -> Context
updateContext (Context ctx) name tp = Context $ M.insert name tp ctx

nameSet :: Type -> Set Name
nameSet (TVar n)     = singleton n
nameSet (TArr t1 t2) = nameSet t1 `S.union` nameSet t2

-- Generate equations set from some term
-- NB: you can use @fresh@ function to generate type names
e :: Context -> Term -> Type -> Maybe [Equation]
e ctx term tpe = snd $ eNamed S.empty ctx term tpe

eNamed :: Set Name -> Context -> Term -> Type -> (Set Name, Maybe [Equation])
eNamed used ctx@(Context ctMap) term tpe = case term of
                   Var{..} -> (var `S.insert` used, pure . (,) tpe <$> ctx ! var)
                   App{..} -> let used' = used `S.union` nameSet tpe `S.union` getCtxNames ctMap
                                  alpha = fresh used'
                                  (used'', eq1) = eNamed (alpha `S.insert` used') ctx algo (TArr (TVar alpha) tpe)
                                  (usedFinal, eq2) = eNamed used'' ctx arg (TVar alpha)
                              in (usedFinal, eq1 `mappend` eq2)
                   Lam{..} -> let tpeNames   = nameSet tpe
                                  used'      = used `S.union` tpeNames `S.union` getCtxNames ctMap
                                  alpha      = fresh used'
                                  updatedCtx = updateContext ctx variable (TVar alpha)
                                  used''     = used' `S.union` getCtxNames (getCtx updatedCtx)
                                  beta       = fresh used''
                                  arrAB      = TArr (TVar alpha) (TVar beta)
                                  (usedFinal, eq) = eNamed (beta `S.insert` used'') updatedCtx body (TVar beta)
                              in (usedFinal, eq `mappend` pure [(tpe, arrAB)])

-- Find a principal pair of some term if exists
pp :: Term -> Maybe (Context, Type)
pp term = do let ctx = contextFromTerm term
             let tpe = TVar "r"
             eqs <- e ctx term tpe
             subs <- u eqs
             pure (substitute subs ctx, substitute subs tpe)
