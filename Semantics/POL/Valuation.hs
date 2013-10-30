module Semantics.POL.Valuation where
import Language.POL.Syntax
import Data.POL.PersonalData
import Data.POL.Purpose
import Data.POL.Observable
import Control.Monad ( liftM2 )
class (Monad m, Functor m) => Valuation m where
  selfInformation :: (PersonalData a, Purpose p)
                  => a -> p -> m Double
  ifthenelse      :: Label l
                  => ObservableT l m Bool -> m Double -> m Double
                  -> m Double
  discount        :: Label l
                  => ObservableT l m Bool -> m Double -> m Double
  snell           :: Label l
                  => ObservableT l m Bool -> m Double -> m Double
  absorb          :: Label l
                  => ObservableT l m Bool -> m Double -> m Double
valuate               ::  ( Valuation m
                          , PersonalData a, Purpose p, Label l
                          , Show a, Show p, Show l
                          , Ord a, Ord p, Ord l
                          )
                      => Contract a p l m -> m Double
valuate Zero          = return 0
valuate (Data a p)    = selfInformation a p
valuate (Give c)      = fmap negate (valuate c)
valuate (And c1 c2)   = liftM2 (+) (valuate c1) (valuate c2)
valuate (Or c1 c2)    = liftM2 max (valuate c1) (valuate c2)
valuate (If o c)      = ifthenelse  o (valuate c) (return 0)
valuate (IfNot o c)   = ifthenelse  o (return 0) (valuate c)
valuate (When o c)    = discount    o (valuate c)
valuate (Anytime o c) = snell       o (valuate c)
valuate (Until o c)   = absorb      o (valuate c)
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
