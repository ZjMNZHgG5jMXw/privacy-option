module Data.POL.Time where
class Zero a where
  zero      :: a
class Discrete a where
  advance     :: a -> a
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
