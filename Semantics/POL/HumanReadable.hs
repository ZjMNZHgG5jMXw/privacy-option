module Semantics.POL.HumanReadable
  ( PrettyPrintable, pretty
  , PrettyDriver, text, mark, emph, items, indent, (+++)
  ) where
import Language.POL.Syntax ( Contract ( .. ), foldAnd, foldOr )
import Data.POL.Observable ( ObservableT )
class PrettyPrintable a where
  pretty :: PrettyDriver b => a -> b
class PrettyDriver a where
  text    :: String -> a
  mark    :: String -> a
  emph    :: String -> a
  items   :: [a] -> a
  indent  :: a -> a -> a
  (+++)   :: a -> a -> a
instance (PrettyPrintable a, PrettyPrintable p, Show l)
      => PrettyPrintable (Contract a p l m) where
  pretty (Zero)
    = text "no rights or obligations"
  pretty (Data a p)
    = (text "your ") +++ (pretty a) +++ (text " is used for ")
      +++ (pretty p)
  pretty (Give c)
    = text "your partner's contract"
    `indent` pretty c
  pretty c@(_ `And` _)
    = indent (text "all are binding")
    $ items $ reverse (foldAnd  ((:) . pretty) [] c)
  pretty c@(_ `Or` _)
    = indent (text "choose between")
    $ items $ reverse (foldOr   ((:) . pretty) [] c)
  pretty (If o c)
    = text "in case \'" +++ pretty o +++ text "\' is true"
    `indent` pretty c
  pretty (IfNot o c)
    = text "in case \'" +++ pretty o +++ text "\' is false"
    `indent` pretty c
  pretty (When o c)
    = text "as soon as \'" +++ pretty o +++ text "\' is true"
    `indent` pretty c
  pretty (Anytime o c)
    = text "anytime \'" +++ pretty o
                        +++ text "\' is true, do once"
                        `indent` pretty c
  pretty (Until o c)
    = text "before \'"  +++ pretty o
                        +++ text "\' becomes true, do once"
                        `indent` pretty c
instance Show l => PrettyPrintable (ObservableT l m a) where
  pretty = mark . show
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
