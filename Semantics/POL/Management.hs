{-# LANGUAGE MultiParamTypeClasses #-}
module Semantics.POL.Management where
import Prelude hiding ( and )
import qualified Prelude as P
import Control.Parallel
import Language.POL.Syntax hiding ( when )
import Data.POL.Observable ( ObservableT )
class Monad m => Management a p l m where
  use         :: a -> p -> m (Contract a p l m)
  send        :: Contract a p l m -> m (Contract a p l m)
  greedy      :: Contract a p l m -> Contract a p l m -> m (Contract a p l m)
  ifthenelse  :: ObservableT l m Bool -> Contract a p l m -> Contract a p l m -> m (Contract a p l m)
  when        :: ObservableT l m Bool -> Contract a p l m -> m (Contract a p l m)
  stopping    :: ObservableT l m Bool -> Contract a p l m -> m (Contract a p l m)
  absorb      :: ObservableT l m Bool -> Contract a p l m -> m (Contract a p l m)
execute :: (Management a p l m, Ord a, Ord p, Ord l) => Contract a p l m -> m (Contract a p l m)
execute Zero          = return Zero
execute (Data a p)    = use a p
execute (Give c)      = send c
execute (And c1 c2)   = do
  c1' <- execute c2 `par` execute c1
  c2' <- execute c2
  return $ c1' `and` c2'
execute (c1 `Or` c2)  = greedy c1 c2
execute (If o c)      = ifthenelse o c Zero
execute (IfNot o c)   = ifthenelse o Zero c
execute (When o c)    = when o c
execute (Anytime o c) = stopping o c
execute (Until o c)   = absorb o c
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
