{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module LTL
  ( Machine(..)
  , Reason(..)
  , Result(..)
  , step
  , run
  , neg
  , top
  , bottom
  , accept
  , reject
  , LTL.and
  , LTL.or
  , next
  , LTL.until
  , release
  , implies
  , eventually
  , always
  , truth
  , eq

  , test
  ) where

import GHC.Generics
import Prelude hiding (and, or, until)

-- | This formulation of LTL is in positive normal form by construction, and
--   trivially dualizable. This choice was driven by the following Coq
--   formalization, showing that all the laws for LTL hold under a denotation
--   from this structure into Coq's logic, over all finite lists:
--
--   https://github.com/jwiegley/constructive-ltl/blob/master/src/LTL.v#L69

data Machine a b
  = Stop b
  | Delay (Machine a b)
  | Ask (a -> Machine a b)
  deriving (Generic, Functor)

instance Applicative (Machine a) where
  pure = Stop

  Stop f  <*> Stop x  = Stop (f x)
  Stop f  <*> Delay x = Delay (Stop f <*> x)
  Stop f  <*> Ask x   = Ask $ \a -> Stop f <*> x a
  Delay f <*> Stop x  = Delay (f <*> Stop x)
  Delay f <*> Delay x = Delay (f <*> x)
  Delay f <*> Ask x   = Ask $ \a -> Delay f <*> x a
  Ask f   <*> Stop x  = Ask $ \a -> f a <*> Stop x
  Ask f   <*> Delay x = Ask $ \a -> f a <*> Delay x
  Ask f   <*> Ask x   = Ask $ \a -> f a <*> x a

instance Monad (Machine a) where
  return = pure
  Stop m  >>= f = f m
  Delay m >>= f = Delay (m >>= f)
  Ask m   >>= f = Ask $ \a -> m a >>= f

step :: Machine a b -> a -> Machine a b
step m x = case m of
  Stop r  -> Stop r
  Delay n -> n
  Ask f   -> step (f x) x

run :: Machine a b -> [a] -> Machine a b
run m = \case
  []     -> m
  x : xs -> run (step m x) xs

data Reason a
  = HitBottom   String
  | Rejected    a
  | BothFailed  (Reason a) (Reason a)
  | LeftFailed  (Reason a)
  | RightFailed (Reason a)
  deriving (Generic, Show)

data Result a
  = Failure (Reason a)
  | Success
  deriving (Generic, Show)

type LTL a = Machine a (Result a)

combine :: LTL a -> LTL a -> LTL a
combine (Stop (Failure e)) _  = Stop (Failure (LeftFailed e))
combine _ (Stop (Failure e))  = Stop (Failure (RightFailed e))
combine (Delay f') (Delay g') = Delay (combine f' g')
combine (Ask f') (Ask g')     = Ask (\a -> combine (f' a) (g' a))
combine f (Ask g')            = Ask (\a -> combine f (g' a))
combine (Ask f') g            = Ask (\a -> combine (f' a) g)
combine f' (Stop Success)     = f'
combine (Stop Success) g'     = g'

select :: LTL a -> LTL a -> LTL a
select (Stop (Failure e1))
       (Stop (Failure e2))   = Stop (Failure (BothFailed e1 e2))
select (Stop Success) _      = Stop Success
select _ (Stop Success)      = Stop Success
select (Delay f') (Delay g') = Delay (select f' g')
select (Ask f') (Ask g')     = Ask (\a -> select (f' a) (g' a))
select f (Ask g')            = Ask (\a -> select f (g' a))
select (Ask f') g            = Ask (\a -> select (f' a) g)
select (Stop (Failure _)) g' = g'
select f' (Stop (Failure _)) = f'

neg :: LTL a -> LTL a
neg = fmap $ \case
  Success   -> Failure (HitBottom "neg")
  Failure _ -> Success

top :: LTL a
top = Stop Success
{-# INLINE top #-}

bottom :: String -> LTL a
bottom e = Stop (Failure (HitBottom e))
{-# INLINE bottom #-}

accept :: (a -> LTL a) -> LTL a
accept = Ask
{-# INLINE accept #-}

reject :: (a -> LTL a) -> LTL a
reject = neg . Ask
{-# INLINE reject #-}

and :: LTL a -> LTL a -> LTL a
and = combine
{-# INLINE and #-}

or :: LTL a -> LTL a -> LTL a
or = select
{-# INLINE or #-}

next :: LTL a -> LTL a
next = Delay
{-# INLINE next #-}

until :: LTL a -> LTL a -> LTL a
until p q = or q (and p (next (until p q)))
{-# INLINE until #-}

release :: LTL a -> LTL a -> LTL a
release p q = and q (or p (next (release p q)))
{-# INLINE release #-}

implies :: LTL a -> LTL a -> LTL a
implies p = or (neg p)
{-# INLINE implies #-}

eventually :: LTL a -> LTL a
eventually = until top
{-# INLINE eventually #-}

always :: LTL a -> LTL a
always = release (bottom "always")
{-# INLINE always #-}

truth :: Bool -> LTL a
truth b = if b then top else bottom "truth"
{-# INLINE truth #-}

eq :: Eq a => a -> LTL a
eq n = accept $ truth . (== n)
{-# INLINE eq #-}

test :: Machine Int (Result Int)
test = run (always (implies (eq 2) (bottom "here"))) [1,2,3,4]
