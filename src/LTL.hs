{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module LTL
  ( LTL
  , Machine(..)
  , Reason(..)
  , Result(..)
  , run
  , showResult

  , neg
  , top
  , bottom
  , accept
  , reject
  , LTL.and
  , LTL.or
  , next
  , LTL.until
  , weakUntil
  , release
  , strongRelease
  , implies
  , eventually
  , always
  , truth
  , test
  , is
  , eq

  , All(..)
  , Any(..)
  ) where

import Control.DeepSeq
import Data.Function (fix)
import Data.List (foldl')
import GHC.Generics
import Prelude hiding (and, or, until)

newtype Machine a b = Machine { step :: a -> Either (Machine a b) b }

instance Functor (Machine a) where
  fmap f (Machine k) = Machine $ either (Left . fmap f) (Right . f) . k

run :: Machine a b -> [a] -> Either (Machine a b) b
run = foldl' (either step (const . Right)) . Left
{-# INLINE run #-}

data Reason a
  = HitBottom   String
  | Rejected    a
  | BothFailed  (Reason a) (Reason a)
  | LeftFailed  (Reason a)
  | RightFailed (Reason a)
  deriving (Show, Generic, NFData)

data Result a
  = Failure (Reason a)
  | Success
  deriving (Show, Generic, NFData)

type LTL a = Machine a (Result a)

combine :: LTL a -> LTL a -> LTL a
combine (Machine f) g = Machine $ \a ->
  case f a of
    Right (Failure e) -> Right (Failure (LeftFailed e))
    Right Success     -> step g a
    Left f'           -> case step g a of
      Right (Failure e) -> Right (Failure (RightFailed e))
      Right Success     -> Left f'
      Left g'           -> Left $! combine f' g'

select :: LTL a -> LTL a -> LTL a
select (Machine f) g = Machine $ \a ->
  case f a of
    Right Success      -> Right Success
    Right (Failure e1) -> case step g a of
      Right (Failure e2) -> Right (Failure (BothFailed e1 e2))
      g'                 -> g'
    Left f' -> case step g a of
      Right Success     -> Right Success
      Right (Failure _) -> Left f'
      Left g'           -> Left $! select f' g'

showResult :: Show a => Either (LTL a) (Result a) -> String
showResult (Left _)  = "<need input>"
showResult (Right b) = show b
{-# INLINE showResult #-}

invert :: Result a -> Result a
invert = \case
  Success   -> Failure (HitBottom "neg")
  Failure _ -> Success
{-# INLINE invert #-}

neg :: LTL a -> LTL a
neg = fmap invert
{-# INLINE neg #-}

stop :: Result a -> LTL a
stop = Machine . const . Right
{-# INLINE stop #-}

top :: LTL a
top = stop Success
{-# INLINE top #-}

bottom :: String -> LTL a
bottom = stop . Failure . HitBottom
{-# INLINE bottom #-}

accept :: (a -> LTL a) -> LTL a
accept f = Machine $ \a -> step (f a) a
{-# INLINE accept #-}

reject :: (a -> LTL a) -> LTL a
reject = neg . accept
{-# INLINE reject #-}

and :: LTL a -> LTL a -> LTL a
and = combine
{-# INLINE and #-}

or :: LTL a -> LTL a -> LTL a
or = select
{-# INLINE or #-}

next :: LTL a -> LTL a
next = Machine . const . Left
{-# INLINE next #-}

until :: LTL a -> LTL a -> LTL a
until p q = fix $ or q . combine p . next
{-# INLINE until #-}

weakUntil :: LTL a -> LTL a -> LTL a
weakUntil p q = (p `until` q) `or` always p
{-# INLINE weakUntil #-}

release :: LTL a -> LTL a -> LTL a
release p q = fix $ and q . select p . next
{-# INLINE release #-}

strongRelease :: LTL a -> LTL a -> LTL a
strongRelease p q = (p `release` q) `and` eventually p
{-# INLINE strongRelease #-}

implies :: LTL a -> LTL a -> LTL a
implies = or . neg
{-# INLINE implies #-}

eventually :: LTL a -> LTL a
eventually = until top
{-# INLINE eventually #-}

always :: LTL a -> LTL a
always = release (bottom "always")
{-# INLINE always #-}

truth :: Bool -> LTL a
truth True  = top
truth False = bottom "truth"
{-# INLINE truth #-}

is :: (a -> Bool) -> LTL a
is = accept . (truth .)
{-# INLINE is #-}

test :: (a -> Bool) -> LTL a
test = is
{-# INLINE test #-}

eq :: Eq a => a -> LTL a
eq = is . (==)
{-# INLINE eq #-}

newtype All a = All { getAll :: LTL a }

instance Semigroup (All a) where
  All x <> All y = All (combine x y)
  {-# INLINE (<>) #-}

instance Monoid (All a) where
  mempty = All top
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

newtype Any a = Any { getAny :: LTL a }

instance Semigroup (Any a) where
  Any x <> Any y = Any (select x y)
  {-# INLINE (<>) #-}

instance Monoid (Any a) where
  mempty = Any (bottom "mempty")
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
