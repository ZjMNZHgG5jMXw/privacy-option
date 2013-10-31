{-# LANGUAGE MultiParamTypeClasses #-}
module Semantics.POL.Valuation where
import Language.POL.Syntax
import Data.POL.Observable ( ObservableT )
import Control.Monad ( liftM2 )
class Monad m => Valuation a p l m where
  selfInformation :: Contract a p l m -> m Double
  ifthenelse      :: ObservableT l m Bool -> Contract a p l m -> Contract a p l m -> m Double
  discount        :: ObservableT l m Bool -> Contract a p l m -> m Double
  snell           :: ObservableT l m Bool -> Contract a p l m -> m Double
  absorb          :: ObservableT l m Bool -> Contract a p l m -> m Double
valuate :: (Functor m, Valuation a p l m) => Contract a p l m -> m Double
valuate Zero          = return 0
valuate c@(Data _ _)  = selfInformation c
valuate (Give c)      = fmap negate (valuate c)
valuate (And c1 c2)   = liftM2 (+) (valuate c1) (valuate c2)
valuate (Or c1 c2)    = liftM2 max (valuate c1) (valuate c2)
valuate (If o c)      = ifthenelse  o c Zero
valuate (IfNot o c)   = ifthenelse  o Zero c
valuate (When o c)    = discount    o c
valuate (Anytime o c) = snell       o c
valuate (Until o c)   = absorb      o c
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
