module Data.POL.Observable where
import Prelude hiding
  ( (==), (/=)
  , (<=), (>=), (<), (>)
  , (&&), (||)
  , not
  )
import qualified Prelude as P
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
data ObservableT l m a = ObservableT
  { label           :: l
  , runObservableT  :: m a
  }
instance Show l => Show (ObservableT l m a) where
  show = show . label
class Label l where
  nolabel :: l
  nullary :: String -> l
  unary   :: String -> l -> l
  binary  :: String -> l -> l -> l
instance Label l => MonadTrans (ObservableT l) where
  lift m = ObservableT
    { label           = nullary "lifted"
    , runObservableT  = m
    }
instance (Label l, MonadIO m) => MonadIO (ObservableT l m) where
  liftIO = lift . liftIO
instance (Label l, Monad m) => Monad (ObservableT l m) where
  return a            = ObservableT
    { label           = nullary "constant"
    , runObservableT  = return a
    }
  m >>= f             = ObservableT
    { label           = unary "modified" (label m)
    , runObservableT  = runObservableT . f =<< runObservableT m
    }
  m >> n              = ObservableT
    { label           = binary ">>" (label m) (label n)
    , runObservableT  = runObservableT m >> runObservableT n
    }
  fail str            = ObservableT
    { label           = nullary "failed"
    , runObservableT  = fail str
    }
labeled :: l -> ObservableT l m a -> ObservableT l m a
labeled l m = m { label = l }
relabel :: (l -> l) -> ObservableT l m a -> ObservableT l m a
relabel f m = m { label = f (label m) }
constant :: (Label l, Monad m, Show a) => a -> ObservableT l m a
constant a = labeled (nullary (show a)) (return a)
instance Functor m => Functor (ObservableT l m) where
  fmap f obs = obs { runObservableT = fmap f (runObservableT obs) }
instance Eq l => Eq (ObservableT l m a) where
  m == n = label m P.== label n
instance Ord l => Ord (ObservableT l m a) where
  compare m n = compare (label m) (label n)
instance (Label l, Monad m, Num a) => Num (ObservableT l m a) where
  m + n         = labeled (binary "+" (label m) (label n))
                    (liftM2 (+) m n)
  m - n         = labeled (binary "-" (label m) (label n))
                    (liftM2 (-) m n)
  m * n         = labeled (binary "*" (label m) (label n))
                    (liftM2 (*) m n)
  abs m         = labeled (unary "abs" (label m))
                    (liftM abs m)
  signum m      = labeled (unary "signum" (label m))
                    (liftM signum m)
  fromInteger i = labeled (nullary (show i))
                    (return (fromInteger i))
infix 4 ==
(==)  :: (Label l, Monad m, Eq a)
      => ObservableT l m a -> ObservableT l m a
      -> ObservableT l m Bool
m == n  = labeled (binary "==" (label m) (label n))
            (liftM2 (P.==) m n)
infix 4 /=
(/=)  :: (Label l, Monad m, Eq a)
      => ObservableT l m a -> ObservableT l m a
      -> ObservableT l m Bool
m /= n  = labeled (binary "/=" (label m) (label n))
            (not (m == n))
infix 4 <=
(<=)  :: (Label l, Monad m, Ord a)
      => ObservableT l m a -> ObservableT l m a
      -> ObservableT l m Bool
m <= n  = labeled (binary "<=" (label m) (label n))
            (liftM2 (P.<=) m n)
infix 4 >=
(>=)  :: (Label l, Monad m, Ord a)
      => ObservableT l m a -> ObservableT l m a
      -> ObservableT l m Bool
m >= n  = labeled (binary ">=" (label m) (label n))
            (liftM2 (P.>=) m n)
infix 4 <
(<) :: (Label l, Monad m, Ord a)
    => ObservableT l m a -> ObservableT l m a
    -> ObservableT l m Bool
m < n = labeled (binary "<" (label m) (label n))
          (liftM2 (P.<) m n)
infix 4 >
(>) :: (Label l, Monad m, Ord a)
    => ObservableT l m a -> ObservableT l m a
    -> ObservableT l m Bool
m > n = labeled (binary ">" (label m) (label n))
          (liftM2 (P.>) m n)
infix 3 &&
(&&)  :: (Label l, Monad m)
      => ObservableT l m Bool -> ObservableT l m Bool
      -> ObservableT l m Bool
m && n = labeled (binary "&&" (label m) (label n))
            (liftM2 (P.&&) m n)
infix 2 ||
(||)  :: (Label l, Monad m)
      => ObservableT l m Bool -> ObservableT l m Bool
      -> ObservableT l m Bool
m || n = labeled (binary "||" (label m) (label n))
            (liftM2 (P.||) m n)
not :: (Label l, Monad m)
    => ObservableT l m Bool -> ObservableT l m Bool
not m = labeled (unary "not" (label m)) (liftM P.not m)

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
