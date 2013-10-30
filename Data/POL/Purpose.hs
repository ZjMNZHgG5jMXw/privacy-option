module Data.POL.Purpose where
class Purpose a where
  purpose   :: a -> String
  covers    :: a -> a -> Bool
  covers a b = purpose a == purpose b
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
