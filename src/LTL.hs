{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

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

newtype Machine a b = Machine
  { runMachine :: forall r. (b -> r) ->
                      (Machine a b -> r) ->
                      ((a -> Machine a b) -> r) ->
                      r }

instance Functor (Machine a) where
  fmap f (Machine k) = Machine $ \s d a ->
    k (s . f) (d . fmap f) (\g -> a (fmap f . g))
  {-# INLINE fmap #-}

step :: Machine a b -> a -> Machine a b
step (Machine k) x = Machine $ \s d a ->
  k s (\(Machine m) -> m s d a)
      (\g -> runMachine (step (g x) x) s d a)
{-# INLINE step #-}

stop :: b -> Machine a b
stop b = Machine $ \s _ _ -> s b
{-# INLINE stop #-}

delay :: Machine a b -> Machine a b
delay m = Machine $ \_ d _ -> d m
{-# INLINE delay #-}

ask :: (a -> Machine a b) -> Machine a b
ask f = Machine $ \_ _ a -> a f
{-# INLINE ask #-}

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
combine (Machine f) (Machine g) = Machine $ \s d a ->
  f (\case Failure e -> s (Failure (LeftFailed e))
           Success   ->
             g (\case Failure e -> s (Failure (RightFailed e))
                      Success   -> s Success)
               d
               a)
    (\f' -> g (\case Failure e -> s (Failure (RightFailed e))
                     Success   -> d f')
             (\g' -> runMachine (combine f' g') s d a)
             (\h  -> a $ \x -> combine (delay f') (h x)))
    (\k  -> g (\case Failure e -> s (Failure (RightFailed e))
                     Success   -> a k)
             (\g' -> a $ \x -> combine (k x) (delay g'))
    (\h -> a $ \x -> combine (k x) (h x)))

select :: LTL a -> LTL a -> LTL a
select (Machine f) (Machine g) = Machine $ \s d a ->
  f (\case Success    -> s Success
           Failure e1 ->
             g (\case Failure e2 -> s (Failure (BothFailed e1 e2))
                      Success    -> s Success)
               d
               a)
    (\f' -> g (\case Failure _ -> d f'
                     Success   -> s Success)
             (\g' -> runMachine (select f' g') s d a)
             (\h  -> a $ \x -> select (delay f') (h x)))
    (\k  -> g (\case Failure _ -> a k
                     Success   -> s Success)
             (\g' -> a $ \x -> select (k x) (delay g'))
    (\h -> a $ \x -> select (k x) (h x)))

neg :: LTL a -> LTL a
neg = fmap $ \case
  Success   -> Failure (HitBottom "neg")
  Failure _ -> Success
{-# INLINE neg #-}

top :: LTL a
top = stop Success
{-# INLINE top #-}

bottom :: String -> LTL a
bottom e = stop (Failure (HitBottom e))
{-# INLINE bottom #-}

accept :: (a -> LTL a) -> LTL a
accept = ask
{-# INLINE accept #-}

reject :: (a -> LTL a) -> LTL a
reject f = ask $ neg . f
{-# INLINE reject #-}

and :: LTL a -> LTL a -> LTL a
and !p !q = combine p q
{-# INLINE and #-}

or :: LTL a -> LTL a -> LTL a
or !p = select p
{-# INLINE or #-}

next :: LTL a -> LTL a
next = delay
{-# INLINE next #-}

until :: LTL a -> LTL a -> LTL a
until p q = go
  where
  go = or q (and p (next go))
  {-# INLINE go #-}
{-# INLINE until #-}

release :: LTL a -> LTL a -> LTL a
release p q = go
  where
  go = and q (or p (next go))
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

eq :: Eq a => a -> LTL a
eq n = accept $ truth . (== n)
{-# INLINE eq #-}

weakUntil :: LTL a -> LTL a -> LTL a
weakUntil p q = (p `until` q) `and` always p
{-# INLINE weakUntil #-}

strongRelease :: LTL a -> LTL a -> LTL a
strongRelease p q = q `until` (p `or` q)
{-# INLINE strongRelease #-}

test :: (a -> Bool) -> LTL a
test f = accept $ truth . f
{-# INLINE test #-}
