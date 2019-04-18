{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module LTL
  ( LTL(..)
  , Machine(..)
  , Reason(..)
  , Result(..)
  , compile
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
  ) where

import GHC.Generics

-- | This formulation of LTL is in positive normal form by construction, and
--   trivially dualizable. This choice was driven by the following Coq
--   formalization, showing that all the laws for LTL hold under a denotation
--   from this structure into Coq's logic, over all finite lists:
--
--   https://github.com/jwiegley/constructive-ltl/blob/master/src/LTL.v#L69
data LTL a
  = Top
  | Bottom  String
  | Accept  (a -> LTL a)
  | Reject  (a -> LTL a)
  | And     (LTL a) (LTL a)
  | Or      (LTL a) (LTL a)
  | Next    (LTL a)
  | Until   (LTL a) (LTL a)
  | Release (LTL a) (LTL a)
  deriving Generic

-- jww (2019-04-17): Write this correctly
instance Show (LTL a) where
  show = \case
    Top         -> "Top"
    Bottom e    -> "Bottom " ++ show e
    Accept _    -> "Accept"
    Reject _    -> "Reject"
    And p q     -> "And " ++ show p ++ " " ++ show q
    Or  p q     -> "Or " ++ show p ++ " " ++ show q
    Next p      -> "Next " ++ show p
    Until   p q -> "Until " ++ show p ++ " " ++ show q
    Release p q -> "Release " ++ show p ++ " " ++ show q

data Machine a b
  = Stop b
  | Delay (Machine a b)
  | Query (a -> Machine a b)
  deriving (Generic, Functor)

instance Show b => Show (Machine a b) where
  show = \case
    Stop b  -> "Stop (" ++ show b ++ ")"
    Delay m -> "Delay (" ++ show m ++ ")"
    Query _ -> "Query"

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

combine :: Machine a (Result a) -> Machine a (Result a)
        -> Machine a (Result a)
combine (Stop (Failure e)) _  = Stop (Failure (LeftFailed e))
combine _ (Stop (Failure e))  = Stop (Failure (RightFailed e))
combine (Delay f') (Delay g') = Delay (combine f' g')
combine (Query f') (Query g') = Query (\a -> combine (f' a) (g' a))
combine f (Query g')          = Query (\a -> combine f (g' a))
combine (Query f') g          = Query (\a -> combine (f' a) g)
combine f' (Stop Success)     = f'
combine (Stop Success) g'     = g'

select :: Machine a (Result a) -> Machine a (Result a)
       -> Machine a (Result a)
select (Stop (Failure e1))
       (Stop (Failure e2))   = Stop (Failure (BothFailed e1 e2))
select (Stop Success) _      = Stop Success
select _ (Stop Success)      = Stop Success
select (Delay f') (Delay g') = Delay (select f' g')
select (Query f') (Query g') = Query (\a -> select (f' a) (g' a))
select f (Query g')          = Query (\a -> select f (g' a))
select (Query f') g          = Query (\a -> select (f' a) g)
select (Stop (Failure _)) g' = g'
select f' (Stop (Failure _)) = f'

compile :: LTL a -> Machine a (Result a)
compile = \case
  Top         -> Stop Success
  Bottom e    -> Stop (Failure (HitBottom e))

  Accept v    -> Query (compile . v)
  Reject v    -> Query $ \x ->
    fmap (\case Failure _ -> Success
                Success   -> Failure (Rejected x))
         (compile (v x))

  And p q     -> combine (compile p) (compile q)
  Or p q      -> select (compile p) (compile q)

  Next p      -> Delay (compile p)

  Until p q   -> compile (Or q (And p (Next (Until p q))))
  Release p q -> compile (And q (Or p (Next (Release p q))))

neg :: LTL a -> LTL a
neg = \case
  Top         -> Bottom "neg"
  Bottom _    -> Top
  Accept v    -> Reject v
  Reject v    -> Accept v
  And p q     -> Or (neg p) (neg q)
  Or p q      -> And (neg p) (neg q)
  Next p      -> Next (neg p)
  Until p q   -> Release (neg p) (neg q)
  Release p q -> Until (neg p) (neg q)

step :: Machine a b -> a -> Machine a b
step m x = case m of
  Stop r  -> Stop r
  Delay n -> n
  Query f -> step (f x) x

run :: Machine a b -> [a] -> Machine a b
run m = \case
  []     -> m
  x : xs -> run (step m x) xs

top :: LTL a
top = Top

bottom :: LTL a
bottom = Bottom "bottom"

accept :: (a -> LTL a) -> LTL a
accept = Accept

reject :: (a -> LTL a) -> LTL a
reject = Reject

and :: LTL a -> LTL a -> LTL a
and = And

or :: LTL a -> LTL a -> LTL a
or = Or

next :: LTL a -> LTL a
next = Next

until :: LTL a -> LTL a -> LTL a
until = Until

release :: LTL a -> LTL a -> LTL a
release = Release

implies :: LTL a -> LTL a -> LTL a
implies p q = Or (neg p) q

eventually :: LTL a -> LTL a
eventually p = Until Top p

always :: LTL a -> LTL a
always p = Release (Bottom "always") p

truth :: Bool -> LTL a
truth b = if b then Top else Bottom "truth"

eq :: Eq a => a -> LTL a
eq n = Accept $ truth . (== n)

test :: Machine Int (Result Int)
test = run (compile (implies (eq 2) (Bottom "here"))) [2]
