module Main where

import Prelude hiding ( and, or, until )
import Language.SimPOL
import Language.SimPOL.Template
import System.Exit

checks :: [(String, Bool)]
checks =
  [ ("c /= d",                                                      c1 /= c2)
  , ("c /= e",                                                      c1 /= c3)
  , ("d /= e",                                                      c2 /= c3)
  , ("give zero == zero",                                           give z == Zero)
  , ("give (give c) == c",                                          give (give c1) == c1)
  , ("c `and` zero == zero",                                        (c1 `and` zero) == c1)
  , ("(give c) `and` (give d) == give (c `and` d)",                 (give c1 `and` give c2) == Give (c1 `And` c2))
  , ("(ifobs o c) `and` (ifnot o c) == c",                          (ifobs o1 c1 `and` ifnot o1 c1) == c1)
  , ("c `or` c == c",                                               (c1 `or` c1) == c1)
  , ("c `and` d == d `and` c",                                      (c2 `and` c1) == (c1 `And` c2))
  , ("c `or` d == d `or` c",                                        (c2 `or` c1) == (c1 `Or` c2))
  , ("c `or` (c `and` d) == c `and` (zero `or` d)",                 (c1 `or` (c1 `and` c2)) == (c1 `And` (Zero `Or` c2)))
  , ("(c `and` d) `or` (c `and` e) == c `and` (d `or` e)",          ((c1 `and` c2) `or` (c1 `and` c3)) == (c1 `And` (c2 `Or` c3)))
  , ("(c `and` d) `or` (e `and` d) == d `and` (c `or` e)",          ((c1 `and` c2) `or` (c3 `and` c2)) == (c2 `And` (c1 `Or` c3)))
  , ("c `or` d `or` (c `and` d) == c `or` (d `and` (zero `or` c)",  (c1 `or` c2 `or` (c1 `and` c2)) == (c1 `Or` (c2 `And` (Zero `Or` c1))))
  , ("d `or` c `or` (c `and` d) == c `or` (d `and` (zero `or` c)",  (c2 `or` c1 `or` (c1 `and` c2)) == (c1 `Or` (c2 `And` (Zero `Or` c1))))
  , ("c `or` c `and` d `or` c `and` d `and` e == c `and` (zero `or` d `and` (zero `or` e)",
                                                                    (c1 `or` c1 `and` c2 `or` c1 `and` c2 `and` c3) == (c1 `And` (Zero `Or` c2 `And` (Zero `Or` c3))))
  , ("ifobs o (give c) == give (ifobs o c)",                        ifobs o1 (give c1) == Give (If o1 c1))
  , ("ifnot o (give c) == give (ifnot o c)",                        ifnot o1 (give c1) == Give (IfNot o1 c1))
  , ("ifobs o (c `and` d) == (ifobs o c) `and` (ifobs o d)",        ifobs o1 (c1 `and` c2) == (If o1 c1 `And` If o1 c2))
  , ("ifnot o (c `and` d) == (ifnot o c) `and` (ifnot o d)",        ifnot o1 (c1 `and` c2) == (IfNot o1 c1 `And` IfNot o1 c2))
  , ("ifobs o (c `or` d) == (ifobs o c) `or` (ifobs o d)",          ifobs o1 (c1 `or` c2) == (If o1 c1 `Or` If o1 c2))
  , ("ifnot o (c `or` d) == (ifnot o c) `or` (ifnot o d)",          ifnot o1 (c1 `or` c2) == (IfNot o1 c1 `Or` IfNot o1 c2))
  , ("ifobs o (ifobs o c) == ifobs o c",                            ifobs o1 (ifobs o1 c1) == If o1 c1)
  , ("ifobs o (ifnot o c) == zero",                                 ifobs o1 (ifnot o1 c1) == Zero)
  , ("ifnot o (ifobs o c) == zero",                                 ifnot o1 (ifobs o1 c1) == Zero)
  , ("ifnot o (ifnot o c) == ifobs o c",                            ifnot o1 (ifnot o1 c1) == IfNot o1 c1)
  , ("ifobs o (when o c) == ifobs o c",                             ifobs o1 (when o1 c1) == If o1 c1)
  , ("ifobs o (until o c) == ifobs o c",                            ifobs o1 (until o1 c1) == Zero)
  , ("ifnot o (until o c) == until o c",                            ifnot o1 (until o1 c1) == Until o1 c1)
  , ("when o zero == zero",                                         when o1 z == Zero)
  , ("when o (give c) == give (when o c)",                          when o1 (give c1) == Give (When o1 c1))
  , ("when o (c `and` d) == (when o c) `and` (when o d)",           when o1 (c1 `and` c2) == (When o1 c1 `And` When o1 c2))
  , ("when o (ifobs o c) == when o c",                              when o1 (ifobs o1 c1) == When o1 c1)
  , ("when o (ifnot o c) == zero",                                  when o1 (ifnot o1 c1) == Zero)
  , ("when o (when o c) == when o c",                               when o1 (when o1 c1) == When o1 c1)
  , ("when o (anytime o c) == anytime o c",                         when o1 (anytime o1 c1) == Anytime o1 c1)
  , ("when o (until o c) == zero",                                  when o1 (until o1 c1) == Zero)
  , ("anytime o zero == zero",                                      anytime o1 z == Zero)
  , ("anytime o (ifobs o c) == anytime o c",                        anytime o1 (ifobs o1 c1) == Anytime o1 c1)
  , ("anytime o (ifnot o c) == zero",                               anytime o1 (ifnot o1 c1) == Zero)
  , ("anytime o (when o c) == anytime o c",                         anytime o1 (when o1 c1) == Anytime o1 c1)
  , ("anytime o (anytime o c) == anytime o c",                      anytime o1 (anytime o1 c1) == Anytime o1 c1)
  , ("anytime o (until o c) == zero",                               anytime o1 (until o1 c1) == Zero)
  , ("until o zero == zero",                                        until o1 z == Zero)
  , ("until o (ifobs o c) == zero",                                 until o1 (ifobs o1 c1) == Zero)
  , ("until o (ifnot o c) == until o c",                            until o1 (ifnot o1 c1) == Until o1 c1)
  , ("until o (when o c) == zero",                                  until o1 (when o1 c1) == Zero)
  , ("until o (anytime o c) == zero",                               until o1 (anytime o1 c1) == Zero)
  , ("until o (until o c) == until o c",                            until o1 (until o1 c1) == Until o1 c1)
  ]

main :: IO ExitCode
main  | fs == []  = exitSuccess
      | otherwise = putStr es >> exitFailure
      where fs = failed checks
            es = unlines $ map ((++ " failed.") . show) fs

failed :: [(a, Bool)] -> [a]
failed = map fst . filter (not . snd)

z :: Contract
z = zero

d :: String -> Contract
d x = disclose x x x

c1,c2,c3,c4,c5 :: Contract
c1 = d "a"
c2 = d "b"
c3 = d "c"
c4 = d "d"
c5 = d "e"

o1,o2,o3,o4,o5 :: Obs Bool
o1 = at 1
o2 = at 2
o3 = at 3
o4 = at 4
o5 = at 5

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
