{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.DeepSeq
import Data.Function (fix)
import Data.List (foldl')
import GHC.Generics
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
  deriving (Show, Generic, NFData)

data Result a
  = Failure (Reason a)
  | Success
  deriving (Show, Generic, NFData)

type LTL a = Machine a (Result a)

combine :: LTL a -> LTL a -> LTL a
combine (Stop x) g = case x of
  Failure e -> Stop (Failure (LeftFailed e))
  Success   -> g

combine f@(Delay f') g = case g of
  Stop (Failure e) -> Stop (Failure (RightFailed e))
  Stop Success     -> f
  Delay g'         -> Delay (combine f' g')
  Ask g'           -> Ask (combine f . g')

combine f@(Ask f') g = case g of
  Stop (Failure e) -> Stop (Failure (RightFailed e))
  Stop Success     -> f
  Ask g'           -> Ask $ \a -> combine (f' a) (g' a)
  Delay _          -> Ask $ \a -> combine (f' a) g

combineDelay :: LTL a -> LTL a -> LTL a
combineDelay (Stop (Failure e)) _ = Stop (Failure (LeftFailed e))
combineDelay (Delay f') g         = Delay (combine f' g)
combineDelay (Ask f') g           = Ask (\a -> combineDelay (f' a) g)
combineDelay (Stop Success) g     = Delay g
{-# INLINE combineDelay #-}

select :: LTL a -> LTL a -> LTL a
select (Stop x) g = case x of
  Success      -> Stop Success
  Failure e1   -> case g of
    Stop Success      -> Stop Success
    Stop (Failure e2) -> Stop (Failure (BothFailed e1 e2))
    _                 -> g

select f@(Delay f') g = case g of
  Stop Success     -> Stop Success
  Stop (Failure _) -> f
  Delay g'         -> Delay (select f' g')
  Ask g'           -> Ask (\a -> select f (g' a))

select f@(Ask f') g = case g of
  Stop Success     -> Stop Success
  Stop (Failure _) -> f
  Ask g'           -> Ask (\a -> select (f' a) (g' a))
  Delay _          -> Ask (\a -> select (f' a) g)

selectDelay :: LTL a -> LTL a -> LTL a
selectDelay (Stop Success) _     = Stop Success
selectDelay (Delay f') g         = Delay (select f' g)
selectDelay (Ask f') g           = Ask (\a -> selectDelay (f' a) g)
selectDelay (Stop (Failure _)) g = Delay g
{-# INLINE selectDelay #-}

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
and = combine
{-# INLINE and #-}

or :: LTL a -> LTL a -> LTL a
or = select
{-# INLINE or #-}

next :: LTL a -> LTL a
next = Delay
{-# INLINE next #-}

until :: LTL a -> LTL a -> LTL a
until p q = fix $ or q . combineDelay p
{-# INLINE until #-}

weakUntil :: LTL a -> LTL a -> LTL a
weakUntil p q = (p `until` q) `or` always p
{-# INLINE weakUntil #-}

release :: LTL a -> LTL a -> LTL a
release p q = fix $ and q . selectDelay p
{-# INLINE release #-}

strongRelease :: LTL a -> LTL a -> LTL a
strongRelease p q = (p `release` q) `and` eventually p
{-# INLINE strongRelease #-}

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
eq = test . (==)
{-# INLINE eq #-}
