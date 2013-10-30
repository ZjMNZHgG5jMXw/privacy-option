module Language.POL.Syntax
  ( Contract
    ( Zero, Data
    , Give
    , And, Or
    , If, IfNot
    , When, Anytime, Until
    )
  , zero, pdata
  , give
  , and, or
  , ifobs, ifnot
  , when, anytime, until
  , foldAnd, foldOr
  , foldLeaves, mapLeaves
  ) where
import Data.POL.Observable ( ObservableT )
import Prelude hiding ( and, or, until )
import Text.Printf ( printf )
import Control.Monad ( liftM2, mplus )
import Control.Arrow ( first, second )
import qualified Data.Set as Set
  ( empty, null, member
  , insert, delete, deleteFindMax
  , union, fold
  , fromList, toAscList
  )
import qualified Data.Map as Map
  ( empty, member
  , insert, insertWith
  , unionWith
  , foldWithKey
  , toList
  )
import Data.Set ( Set )
import Data.Map ( Map )
infixr 3 `And`
infixr 2 `Or`
data Contract a p l m
  = Zero
  | Data    a p
  | Give                            (Contract a p l m)
  | And     (Contract a p l m)      (Contract a p l m)
  | Or      (Contract a p l m)      (Contract a p l m)
  | If      (ObservableT l m Bool)  (Contract a p l m)
  | IfNot   (ObservableT l m Bool)  (Contract a p l m)
  | When    (ObservableT l m Bool)  (Contract a p l m)
  | Anytime (ObservableT l m Bool)  (Contract a p l m)
  | Until   (ObservableT l m Bool)  (Contract a p l m)
  deriving (Eq, Ord)
instance (Show a, Show p, Show l) => Show (Contract a p l m) where
  show Zero          = "zero"
  show (Data a p)    = printf "pdata %s %s"     (show a)  (show p)
  show (Give c)      = printf "give (%s)"       (show c)
  show (c1 `And` c2) = printf "(%s) `and` (%s)" (show c1) (show c2)
  show (c1 `Or` c2)  = printf "(%s) `or` (%s)"  (show c1) (show c2)
  show (If o c)      = printf "ifobs (%s) (%s)"   (show o)  (show c)
  show (IfNot o c)   = printf "ifnot (%s) (%s)"   (show o)  (show c)
  show (When o c)    = printf "when (%s) (%s)"    (show o)  (show c)
  show (Anytime o c) = printf "anytime (%s) (%s)" (show o)  (show c)
  show (Until o c)   = printf "until (%s) (%s)"   (show o)  (show c)
zero :: Contract a p l m
zero = Zero
pdata :: a -> p -> Contract a p l m
pdata = Data
give :: Contract a p l m -> Contract a p l m
give Zero         = zero                            -- (R1)
give (Give c)     = c                               -- (R2)
give c            = Give c
infixr 3 `and`
and :: (Ord a, Ord p, Ord l)
    => Contract a p l m -> Contract a p l m -> Contract a p l m
c `and` Zero               = c                      -- (R3)
Zero `and` c               = c                      -- (R4)
(Give c1) `and` (Give c2)  = give (c1 `and` c2)     -- (R5)
c1 `and` c2                = sortAnd (c1 `And` c2)  -- (~R6)
infixr 2 `or`
or  :: (Ord a, Ord p, Ord l)
    => Contract a p l m -> Contract a p l m -> Contract a p l m
c `or` d | c == d         = c                       -- (R7)
c1 `or` c2                = sortOr (c1 `Or` c2)     -- new (R9-12)
ifobs :: (Ord a, Ord p, Ord l)
      => ObservableT l m Bool -> Contract a p l m -> Contract a p l m
ifobs _ Zero              = zero
ifobs o (Give c)          = give (ifobs o c)                -- (R13)
ifobs o (c1 `And` c2)     = (ifobs o c1) `and` (ifobs o c2) -- (R15)
ifobs o (c1 `Or` c2)      = (ifobs o c1) `or` (ifobs o c2)  -- (R17)
ifobs o (If p c)    | o == p  = ifobs o c                   -- (R19)
ifobs o (IfNot p _) | o == p  = zero                        -- (R20)
ifobs o (Until p _) | o == p  = zero                        -- (R24)
ifobs o (When p c)  | o == p  = ifobs o c                   -- (R23)
ifobs o c                     = If o c
ifnot :: (Ord a, Ord p, Ord l)
      => ObservableT l m Bool -> Contract a p l m -> Contract a p l m
ifnot _ Zero              = zero
ifnot o (Give c)          = give (ifnot o c)                -- (R14)
ifnot o (c1 `And` c2)     = (ifnot o c1) `and` (ifnot o c2) -- (R16)
ifnot o (c1 `Or` c2)      = (ifnot o c1) `or` (ifnot o c2)  -- (R18)
ifnot o (If p _)    | o == p  = zero                        -- (R21)
ifnot o (IfNot p c) | o == p  = ifnot o c                   -- (R22)
ifnot o (Until p c) | o == p  = until o c                   -- (R25)
ifnot o c                     = IfNot o c
when  :: (Ord a, Ord p, Ord l)
      => ObservableT l m Bool -> Contract a p l m -> Contract a p l m
when = liftM2 (.) flat deep where
  flat _ Zero               = zero                          -- (R26)
  flat o (Give c)           = give (when o c)               -- (R27)
  flat o (c1 `And` c2)      = (when o c1) `and` (when o c2) -- (R28)
  flat o c | allAnytime o c = c                             -- (R32)
  flat o c                  = When o c
  deep o = mapLeaves reduce' where
    reduce' (If    p c) | o == p = c                        -- (R29)
    reduce' (IfNot p _) | o == p = zero                     -- (R30)
    reduce' (When  p c) | o == p = c                        -- (R31)
    reduce' (Until p _) | o == p = zero                     -- (R33)
    reduce'          c           = c
anytime :: (Ord a, Ord p, Ord l)
        => ObservableT l m Bool -> Contract a p l m
        -> Contract a p l m
anytime = liftM2 (.) flat deep where
  flat _ Zero                     = zero                    -- (R34)
  flat o c                        = Anytime o c
  deep o = mapLeaves reduce' where
    reduce' (If      p c) | o == p = c                      -- (R36)
    reduce' (IfNot   p _) | o == p = zero                   -- (R37)
    reduce' (When    p c) | o == p = c                      -- (R38)
    reduce' (Anytime p c) | o == p = c                      -- (R39)
    reduce' (Until   p _) | o == p = zero                   -- (R40)
    reduce'            c           = c
until :: (Ord a, Ord p, Ord l)
      => ObservableT l m Bool -> Contract a p l m -> Contract a p l m
until = liftM2 (.) flat deep where
  flat _ Zero                     = zero                    -- (R41)
  flat o c                        = Until o c
  deep o = mapLeaves reduce' where
    reduce' (If      p _) | o == p = zero                   -- (R43)
    reduce' (IfNot   p c) | o == p = c                      -- (R44)
    reduce' (When    p _) | o == p = zero                   -- (R45)
    reduce' (Anytime p _) | o == p = zero                   -- (R46)
    reduce' (Until   p c) | o == p = c                      -- (R47)
    reduce'            c           = c
foldAnd :: (Contract a p l m -> x -> x) -> x -> Contract a p l m -> x
foldAnd f x (left `And` right)  = foldAnd f (foldAnd f x left) right
foldAnd f x tree                = f tree x
foldOr :: (Contract a p l m -> x -> x) -> x -> Contract a p l m -> x
foldOr f x (left `Or` right)  = foldOr f (foldOr f x left) right
foldOr f x tree               = f tree x
foldLeaves :: (Contract a p l m -> x -> x) -> x -> Contract a p l m -> x
foldLeaves f x (l `And` r)  = foldLeaves f (foldLeaves f x l) r
foldLeaves f x (l `Or` r)   = foldLeaves f (foldLeaves f x l) r
foldLeaves f x tree         = f tree x
allAnytime  :: Eq l
            => ObservableT l m Bool -> Contract a p l m -> Bool
allAnytime o                = foldLeaves isAnytime True where
  isAnytime (Anytime p _) x = x && o == p
  isAnytime            _  _ = False
mapLeaves :: (Ord a, Ord p, Ord l)
          => (Contract a p l m -> Contract a p l m)
          -> Contract a p l m -> Contract a p l m
mapLeaves f (l `And` r) = mapLeaves f l `and` mapLeaves f r
mapLeaves f (l `Or`  r) = mapLeaves f l `or`  mapLeaves f r
mapLeaves f c           = f c
sortAnd :: (Ord a, Ord p, Ord l)
        => Contract a p l m -> Contract a p l m
sortAnd = unfoldAnd . foldAnd consFoldedAnd nullFoldedAnd
data FoldedAnd a p l m = FoldedAnd
  { ifs   :: Map (ObservableT l m Bool, Contract a p l m) (Int, Int)
  , occs  :: Map (Contract a p l m) Int
  }
nullFoldedAnd :: FoldedAnd a p l m
nullFoldedAnd = FoldedAnd
  { ifs       = Map.empty
  , occs      = Map.empty
  }
(+|+)  :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+|+) (a,b) (c,d) = (a+c,b+d)
consFoldedAnd :: (Ord a, Ord p, Ord l)
  => Contract a p l m -> FoldedAnd a p l m -> FoldedAnd a p l m
consFoldedAnd (If o c)    fa
  = fa { ifs  = Map.insertWith (+|+) (o,c) (1,0) (ifs fa)  }
consFoldedAnd (IfNot o c) fa
  = fa { ifs  = Map.insertWith (+|+) (o,c) (0,1) (ifs fa)  }
consFoldedAnd c           fa
  = fa { occs = Map.insertWith (+)      c   1    (occs fa) }
unfoldAnd :: (Ord a, Ord p, Ord l)
          => FoldedAnd a p l m -> Contract a p l m
unfoldAnd fa = (foldr1 And . expand . join) [occs fa, ifs'] where
  ifs' = Map.foldWithKey merge Map.empty (ifs fa)
  merge (o,c) (a,b)
    | a > b     = Map.insert (If o c)     (a-b) . Map.insert c b
    | a < b     = Map.insert (IfNot o c)  (b-a) . Map.insert c a
    | otherwise =                                 Map.insert c a
join :: (Ord a, Num b) => [Map a b] -> Map a b
join = foldr (Map.unionWith (+)) Map.empty
expand :: Map a Int -> [a]
expand = Map.foldWithKey (((++) .) . flip replicate) []
sortOr  :: (Ord a, Ord p, Ord l)
        => Contract a p l m -> Contract a p l m
sortOr = unfoldOr . foldOr consFoldedOr nullFoldedOr
data FoldedOr a p l m = FoldedOr
  { ols   :: Set (Contract a p l m)
  , ands  :: Set (Contract a p l m)
  , als   :: Map (Contract a p l m) Int
  }
nullFoldedOr :: FoldedOr a p l m
nullFoldedOr = FoldedOr
  { ols   = Set.empty
  , ands  = Set.empty
  , als   = Map.empty
  }
consFoldedOr  :: (Ord a, Ord p, Ord l)
              => Contract a p l m -> FoldedOr a p l m
              -> FoldedOr a p l m
consFoldedOr c@(_ `And` _) = fld addAnd . fld addAls
  where fld = flip (flip . foldr) (expandAnd c)
consFoldedOr c             = addOl  c
addAls  :: (Ord a, Ord p, Ord l)
        => Contract a p l m -> FoldedOr a p l m -> FoldedOr a p l m
addAls c r | Set.member c (ands r) = r
addAls c r = foldAnd (liftM2 (.) mvOl addAl) r c
addAnd  :: (Ord a, Ord p, Ord l)
        => Contract a p l m -> FoldedOr a p l m -> FoldedOr a p l m
addAnd c r = r { ands = Set.insert c (ands r) }
addOl :: (Ord a, Ord p, Ord l)
      => Contract a p l m -> FoldedOr a p l m -> FoldedOr a p l m
addOl c r | Map.member c (als r) = addAl c $ addAnd c r
addOl c r = r { ols  = Set.insert c (ols r) }
addAl :: (Ord a, Ord p, Ord l)
      => Contract a p l m -> FoldedOr a p l m -> FoldedOr a p l m
addAl c r = r { als = Map.insertWith (+) c 1 (als r) }
mvOl  :: (Ord a, Ord p, Ord l)
      => Contract a p l m -> FoldedOr a p l m -> FoldedOr a p l m
mvOl c r  | Set.member c (ols r)
          = addAl c $ addAnd c $ r { ols = Set.delete c (ols r) }
mvOl _ r  = r
expandAnd :: Contract a p l m -> [Contract a p l m]
expandAnd = map (foldr1 And) . expandAnd'
expandAnd' :: Contract a p l m -> [[Contract a p l m]]
expandAnd' (c `And` d)  = do
  cs <- expandAnd' c
  ds <- expandAnd' d
  return (cs ++ ds)
expandAnd' (c `Or` d)   = expandAnd' c `mplus` expandAnd' d
expandAnd' c            = return [c]
unfoldOr    :: (Ord a, Ord p, Ord l)
            => FoldedOr a p l m -> Contract a p l m
unfoldOr r  = foldr1 Or $ Set.toAscList
            $ ols r `Set.union` factors (freq (als r)) (ands r)
freq :: (Ord a, Ord b) => Map a b -> Set (b,a)
freq = Set.fromList . map (uncurry (flip (,))) . Map.toList
factors :: (Ord a, Ord p, Ord l)
        => Set (Int, Contract a p l m) -> Set (Contract a p l m)
        -> Set (Contract a p l m)
factors ls ts
  | Set.null ls = Set.empty
  | Set.null ts = Set.empty
  | otherwise   = maybe lessfreq (flip Set.insert lessfreq) fact
  where
    (leaf, ls') = first snd $ Set.deleteFindMax ls
    (fact, ts') = factor leaf ts
    lessfreq    = factors ls' ts'
factor :: (Ord a, Ord p, Ord l)
      => Contract a p l m -> Set (Contract a p l m)
      -> (Maybe (Contract a p l m), Set (Contract a p l m))
factor leaf = first consTree . Set.fold (reduce leaf) empty where
  consTree ls
    | Set.null ls = Nothing
    | otherwise   = Just $ and leaf $ foldr1 or $ Set.toAscList ls
  empty           = (Set.empty, Set.empty)
reduce :: (Ord a, Ord p, Ord l)
        => Contract a p l m -> Contract a p l m
        -> (Set (Contract a p l m), Set (Contract a p l m))
        -> (Set (Contract a p l m), Set (Contract a p l m))
reduce leaf tree@(And _ _)
  | inTree            = first (Set.insert reducedTree)
  where (inTree, ls)  = foldAnd (red leaf) (False, Set.empty) tree
        reducedTree   = (foldr1 And . Set.toAscList) ls
reduce leaf tree  | leaf == tree  = first   (Set.insert zero)
                  | otherwise     = second  (Set.insert tree)
red :: (Ord a, Ord p, Ord l)
    => Contract a p l m -> Contract a p l m
    -> (Bool, Set (Contract a p l m))
    -> (Bool, Set (Contract a p l m))
red leaf tleaf ls
  | not (fst ls) && tleaf == leaf = first   (const True)        ls
  | otherwise                     = second  (Set.insert tleaf)  ls
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
