{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- | This formulation of LTL is in positive normal form by construction, and
--   trivially dualizable. This choice was driven by the following Coq
--   formalization, showing that all the laws for LTL hold under a denotation
--   from this structure into Coq's logic, over all finite lists:
--
--   https://github.com/jwiegley/constructive-ltl/blob/master/src/LTL.v#L69

module LTL
  ( LTL
  , Machine(..)
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
  , weakUntil
  , release
  , strongRelease
  , implies
  , eventually
  , always
  , truth
  , test
  , eq
  ) where

import Data.List (foldl')
import Prelude hiding (and, or, until)

data Machine a b
  = Stop b
  | Delay (Machine a b)
  | Ask (a -> Machine a b)
  deriving Functor

step :: Machine a b -> a -> Machine a b
step m@(Stop _) _ = m
step (Delay m) _  = m
step (Ask f) x    = step (f x) x
{-# INLINE step #-}

run :: Machine a b -> [a] -> Machine a b
run = foldl' step
{-# INLINE run #-}

data Reason a
  = HitBottom   String
  | Rejected    a
  | BothFailed  (Reason a) (Reason a)
  | LeftFailed  (Reason a)
  | RightFailed (Reason a)
  deriving Show

data Result a
  = Failure (Reason a)
  | Success
  deriving Show

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
{-# INLINE neg #-}

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
reject f = Ask (neg . f)
{-# INLINE reject #-}

and :: LTL a -> LTL a -> LTL a
and !p !q = combine p q
{-# INLINE and #-}

or :: LTL a -> LTL a -> LTL a
or !p = select p
{-# INLINE or #-}

next :: LTL a -> LTL a
next = Delay
{-# INLINE next #-}

until :: LTL a -> LTL a -> LTL a
until p q = go
  where
  go = or q (and p (next go))
  {-# INLINE go #-}
{-# INLINE until #-}

weakUntil :: LTL a -> LTL a -> LTL a
weakUntil p q = (p `until` q) `and` always p
{-# INLINE weakUntil #-}

strongRelease :: LTL a -> LTL a -> LTL a
strongRelease p q = q `until` (p `or` q)
{-# INLINE strongRelease #-}

release :: LTL a -> LTL a -> LTL a
release p q = go
  where
  go = q `and` (p `or` (next go))
  {-# INLINE go #-}
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

test :: (a -> Bool) -> LTL a
test f = accept $ truth . f
{-# INLINE test #-}

eq :: Eq a => a -> LTL a
eq n = accept $ truth . (== n)
{-# INLINE eq #-}
