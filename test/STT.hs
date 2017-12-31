{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Context (..), Name, Substitutable (..),
                               Substitution (..), Term (..), Type (..), compose,
                               pp)
import           Data.Map     (empty, fromList, singleton)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Principal pair tests. Weird terms tested in GHCi." ppTests
  describe "Term and type parser tests." parserTests
  describe "Type equality tests." typeEqTests
  describe "Substitution tests." subTests

ppTests :: Spec -- These are passing iff `e` and `u` both are working properly.
ppTests = do
  it "\\x.x" $ pp "\\x.x" `shouldBe` Just (Context empty, "(t -> t)")
  it "\\x.\\y.x" $ pp "\\x.\\y.x" `shouldBe` Just (Context empty, "(t1 -> (t -> t1))")
  it "\\x.\\y.\\z.\\t.((z (x y)) x)" $ pp "\\x.\\y.\\z.\\t.((z (x y)) x)" `shouldBe` Just (Context empty, "((t3 -> t2) -> (t3 -> ((t2 -> ((t3 -> t2) -> t)) -> (t1 -> t))))")
  it "\\x.\\y.(x (\\z.((y x) z)))" $ pp "\\x.\\y.(x (\\z.((y x) z)))" `shouldBe` Just (Context empty, "(((t1 -> t) -> t2) -> ((((t1 -> t) -> t2) -> (t1 -> t)) -> t2))")
  it "\\x.(x x)" $ pp "\\x.(x x)" `shouldBe` Nothing
  it "((\\x.\\y.\\z.y) a)" $ pp "((\\x.\\y.\\z.y) a)" `shouldBe` Just (Context $ singleton "a" "t0", "(t1 -> (t -> t1))")
  it "(((\\x.\\y.\\z.(((x y) (y z)) z)) a) b)" $ pp "(((\\x.\\y.\\z.(((x y) (y z)) z)) a) b)" `shouldBe` Just (Context $ fromList [("a", "((t2 -> t1) -> (t1 -> (t2 -> t)))"),
                                                                                                                                   ("b", "(t2 -> t1)")],
                                                                                                               "(t2 -> t)")

parserTests :: Spec
parserTests = do
  it "Term x" $ "x" `shouldBe` Var "x"
  it "Term x y" $ "(x y)" `shouldBe` App (Var "x") (Var "y")
  it "Term \\x y -> x" $ "\\x.\\y.x" `shouldBe` Lam "x" (Lam "y" (Var "x"))
  it "Term \\x y -> y (\\t -> f) x" $ "\\x.\\y.((y (\\t.f)) x)" `shouldBe` Lam "x" (Lam "y" (App (App (Var "y") (Lam "t" (Var "f"))) (Var "x")))
  it "Type t" $ "t" `shouldBe` TVar "t"
  it "Type (t0 -> t)" $ "(t0 -> t)" `shouldBe` TArr (TVar "t0") (TVar "t")
  it "Type (a -> (t0 -> f) -> q) -> s -> r4" $ "((a -> ((t0 -> f) -> q)) -> (s -> r4))" `shouldBe`  TArr (TArr (TVar "a") (TArr (TArr (TVar "t0") (TVar "f")) (TVar "q"))) (TArr (TVar "s") (TVar "r4"))

typeEqTests :: Spec
typeEqTests = do
  it "x == y" $ ("x" :: Type) == "y" `shouldBe` True
  it "x /= y -> z" $ ("x" :: Type) == "(y -> z)" `shouldBe` False
  it "c -> d /= t0 -> t0" $ ("(c -> d)" :: Type) == "(t0 -> t0)" `shouldBe` False
  it "(c -> d) -> r /= c -> (d -> r)" $ ("((c -> d) -> r)" :: Type) == "(c -> (d -> r))" `shouldBe` False
  it "(c -> d) -> r == (q -> t0) -> z" $ ("((c -> d) -> r)" :: Type) == "((q -> t0) -> z)" `shouldBe` True

subTests :: Spec
subTests = do
  let subAB = Substitution $ singleton "a" (TVar "b")
  let subCDW = Substitution $ singleton "c" (TArr (TVar "d") (TVar "w"))
  let subDSST = Substitution $ fromList [("d", TVar "s"), ("s", TVar "t")]

  it "Type sub: [a:=b] a" $ substitute subAB ("a" :: Type) `shouldBe` "b"
  it "Type sub: [a:=b] v" $ substitute subAB ("v" :: Type) `shouldBe` "v"
  it "Type sub: [a:=b] p -> a -> q" $ substitute subAB ("(p -> (a -> q))" :: Type) `shouldBe` "(p -> (b -> q))"
  it "Ctx sub: [c:=d -> w] {a:t0, b:c}" $ substitute subCDW (Context $ fromList [("a", "t0"), ("b", "c")]) `shouldBe` (Context $ fromList [("a", "t0"), ("b", "(d -> w)")])
  it "Sub composition in type: [a:=b] . [c:=d -> w] . [d:=s, s:=t] a -> b -> c" $ shouldBe
    (substitute (subAB `compose` subCDW `compose` subDSST) ("(a -> (b -> c))" :: Type))
    (substitute subAB . substitute subCDW . substitute subDSST $ "(a -> (b -> c))")
  it "Sub composition in ctx: [d:=s. s:=t] . [c:=d -> w] . [a:=b] {q:d, p:c -> a}" $ shouldBe
    (substitute (subDSST `compose` subCDW `compose` subAB) (Context $ fromList [("q", "d"), ("p", "(c -> a)")]))
    (substitute subDSST . substitute subCDW . substitute subAB $ (Context $ fromList [("q", "d"), ("p", "(c -> a)")]))
