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
  , release
  , implies
  , eventually
  , always
  , truth
  , eq

  , test
  ) where

import Prelude hiding (and, or, until)

data Machine a b
  = Stop b
  | Delay (Machine a b)
  | Ask (a -> Machine a b)
  deriving Functor

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
{-# INLINABLE neg #-}

top :: LTL a
top = Stop Success
{-# INLINABLE top #-}

bottom :: String -> LTL a
bottom e = Stop (Failure (HitBottom e))
{-# INLINABLE bottom #-}

accept :: (a -> LTL a) -> LTL a
accept = Ask
{-# INLINABLE accept #-}

reject :: (a -> LTL a) -> LTL a
reject = neg . Ask
{-# INLINABLE reject #-}

and :: LTL a -> LTL a -> LTL a
and = combine
{-# INLINABLE and #-}

or :: LTL a -> LTL a -> LTL a
or = select
{-# INLINABLE or #-}

next :: LTL a -> LTL a
next = Delay
{-# INLINABLE next #-}

until :: LTL a -> LTL a -> LTL a
until p q = or q (and p (next (until p q)))
{-# INLINABLE until #-}

release :: LTL a -> LTL a -> LTL a
release p q = and q (or p (next (release p q)))
{-# INLINABLE release #-}

implies :: LTL a -> LTL a -> LTL a
implies p = or (neg p)
{-# INLINABLE implies #-}

eventually :: LTL a -> LTL a
eventually = until top
{-# INLINABLE eventually #-}

always :: LTL a -> LTL a
always = release (bottom "always")
{-# INLINABLE always #-}

truth :: Bool -> LTL a
truth b = if b then top else bottom "truth"
{-# INLINABLE truth #-}

eq :: Eq a => a -> LTL a
eq n = accept $ truth . (== n)
{-# INLINABLE eq #-}

test :: String
test = case go of
  Stop b  -> show b
  Delay _ -> "Delay"
  Ask _   -> "Ask"
 where
  xs = [1,2,3,4] :: [Int]
  go = run (always (implies (eq 2) (bottom "here"))) xs
