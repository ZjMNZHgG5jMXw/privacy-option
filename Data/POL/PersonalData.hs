module Data.POL.PersonalData where
class PersonalData a where
  attribute       :: a -> String
  value           :: a -> String
  covers    :: a -> a -> Bool
  covers a b = attribute  a == attribute  b
            && value      a == value      b
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
