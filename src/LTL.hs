{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | This module provides a simple way of applying LTL formulas to lists, or
--   streams of input, to determine if a given formula matches some or all of
--   that input.
--
--   Formulas are written exactly as you would in LTL, but using names instead
--   of the typical symbols, for example:
--
-- @
-- always (is even @`until@` is odd)
-- @
--
--   Use 'run' to apply a formula to a list of inputs, returning either 'Left'
--   if it needs more input to determine the truth of the formula, or 'Right'
--   if it could determine truth from some prefix of that input.
--
--   Use 'step' to advance a formula by a single input. The return value has
--   the same meaning as 'run', but allows you to apply it in cases whether
--   you don't necessarily have all the inputs at once, such as feeding input
--   gradually within a conduit to check for logic failures.
--
--   For the meaning of almost all the functions in the module, see
--   https://en.wikipedia.org/wiki/Linear_temporal_logic
module LTL
  ( LTL,
    Answer (..),
    run,
    neg,
    top,
    bottom,
    examine,
    accept,
    reject,
    LTL.and,
    LTL.or,
    next,
    orNext,
    andNext,
    LTL.until,
    weakUntil,
    release,
    weakRelease,
    strongRelease,
    implies,
    eventually,
    weakEventually,
    always,
    weakAlways,
    truth,
    test,
    is,
    eq,
  )
where

import Control.DeepSeq
import Data.Function (fix)
import Data.List (foldl')
import GHC.Generics
import Prelude hiding (and, or, until)

data Weakness = Weak | Strong

data Reason a
  = HitBottom String
  | Rejected a
  | BothFailed (Reason a) (Reason a)
  | LeftFailed (Reason a)
  | RightFailed (Reason a)
  deriving (Show, Generic, NFData, Functor)

data PartialAnswer a
  = Abort (Reason a)
  | Failure (Reason a)
  | Continue Weakness (LTL a)
  | Success
  deriving (Generic)

instance Show a => Show (PartialAnswer a) where
  show (Continue _ _) = "<need input>"
  show (Abort res) = "Abort: " ++ show res
  show (Failure res) = "Failure: " ++ show res
  show Success = "Success"
  {-# INLINEABLE show #-}

data Answer a
  = Failed (Reason a)
  | Succeeded
  deriving (Generic, Functor)

finish :: PartialAnswer a -> Answer a
finish = \case
  Abort reason -> Failed reason
  Failure reason -> Failed reason
  Continue weak (LTL formula) -> case formula Nothing weak of
    Abort reason -> Failed reason
    Failure reason -> Failed reason
    Continue Weak _ -> Succeeded
    Continue Strong _ ->
      Failed (HitBottom "Failed to determine formula by end of stream")
    Success -> Succeeded
  Success -> Succeeded
{-# INLINEABLE finish #-}

newtype LTL a = LTL {step :: Maybe a -> Weakness -> PartialAnswer a}

run :: LTL a -> [a] -> Answer a
run formula xs =
  finish $
    foldl'
      ( \acc x -> case acc of
          Continue weak f -> step f (Just x) weak
          res -> res
      )
      (Continue Strong formula)
      xs
{-# INLINEABLE run #-}

-- | ⊤, or "true"
top :: LTL a
top = stop Succeeded
{-# INLINE top #-}

-- | ⊥, or "false"
bottom :: String -> LTL a
bottom = stop . Failed . HitBottom
{-# INLINE bottom #-}

mapAnswer :: (PartialAnswer a -> PartialAnswer a) -> LTL a -> LTL a
mapAnswer f formula = LTL $ \el weak -> f (step formula el weak)
{-# INLINE mapAnswer #-}

-- | Negate a formula: ¬ p
neg :: LTL a -> LTL a
neg = mapAnswer invert
{-# INLINE neg #-}

invert :: PartialAnswer a -> PartialAnswer a
invert = \case
  Success -> Failure (HitBottom "neg")
  Failure _ -> Success
  Abort e -> Abort e
  Continue w f -> Continue w (neg f)
{-# INLINEABLE invert #-}

-- | Boolean conjunction: ∧
and :: LTL a -> LTL a -> LTL a
and (LTL f) g = LTL $ \el weak -> case f el weak of
  Abort e -> Abort (LeftFailed e)
  Failure e -> Failure (LeftFailed e)
  Success -> step g el weak
  Continue w f' -> case step g el w of
    Abort e -> Abort (RightFailed e)
    Failure e -> Failure (RightFailed e)
    Success -> Continue w f'
    Continue w' g' -> Continue w' $! f' `and` g'
{-# INLINEABLE and #-}

andNext :: LTL a -> LTL a -> LTL a
andNext (LTL f) g = LTL $ \el weak -> case f el weak of
  Abort e -> Abort (LeftFailed e)
  Failure e -> Failure (LeftFailed e)
  Success -> Continue weak g
  Continue w f' -> Continue w $! f' `and` g
{-# INLINEABLE andNext #-}

-- | Boolean disjunction: ∨
or :: LTL a -> LTL a -> LTL a
or (LTL f) g = LTL $ \el weak -> case f el weak of
  Success -> Success
  Abort e -> Abort (LeftFailed e)
  Failure e1 -> case step g el weak of
    Failure e2 -> Failure (BothFailed e1 e2)
    g' -> g'
  Continue w f' -> case step g el w of
    Success -> Success
    Abort e -> Abort (RightFailed e)
    Failure _ -> Continue w f'
    Continue w' g' -> Continue w' $! f' `or` g'
{-# INLINEABLE or #-}

orNext :: LTL a -> LTL a -> LTL a
orNext (LTL f) g = LTL $ \el weak -> case f el weak of
  Success -> Success
  Abort e -> Abort (LeftFailed e)
  Failure _ -> Continue weak g
  Continue w f' -> Continue w $! f' `or` g
{-# INLINEABLE orNext #-}

stop :: Answer a -> LTL a
stop x = LTL $ \_ _ -> case x of
  Failed r -> Failure r
  Succeeded -> Success
{-# INLINE stop #-}

-- | Given an input element, provide a formula to determine its truth. These
--   can be nested, making it possible to have conditional formulas. Consider
--   the following:
--
-- @
-- always (examine (\n -> next (eq (succ n))))
-- @
--
--   One way to read this would be: "for every input n, always examine n if its
--   next element is the successor".
examine :: (a -> LTL a) -> LTL a
examine f = LTL $ \el weak -> case el of
  Nothing -> case weak of
    Weak -> Success
    Strong -> Failure (HitBottom "examine has no meaning at end of stream")
  Just a -> step (f a) (Just a) weak
{-# INLINE examine #-}

accept :: (a -> LTL a) -> LTL a
accept = examine
{-# INLINE accept #-}

-- | The opposite in meaning to 'examine', defined simply as 'neg . examine'.
reject :: (a -> LTL a) -> LTL a
reject = neg . examine
{-# INLINE reject #-}

-- | The "next" temporal modality, typically written 'X p' or '◯ p'.
next :: LTL a -> LTL a
next f = LTL $ \el weak -> case el of
  Nothing -> case weak of
    Weak -> Success
    Strong -> Failure (HitBottom "next has no meaning at end of stream")
  Just _ -> Continue weak f
{-# INLINEABLE next #-}

-- | The "until" temporal modality, typically written 'p U q'.
until :: LTL a -> LTL a -> LTL a
until p = \q -> fix $ or q . andNext p
{-# INLINE until #-}

-- | Weak until.
weakUntil :: LTL a -> LTL a -> LTL a
weakUntil p = \q -> LTL $ \el _weak -> case el of
  Nothing -> Success
  Just _ -> step (or q (and p (next (weakUntil p q)))) el Weak
{-# INLINE weakUntil #-}

-- | Release, the dual of 'until'.
release :: LTL a -> LTL a -> LTL a
release p = \q -> fix $ and q . orNext p
{-# INLINE release #-}

-- | Strong release.
strongRelease :: LTL a -> LTL a -> LTL a
strongRelease p = \q -> (p `release` q) `and` eventually p
{-# INLINE strongRelease #-}

weakRelease :: LTL a -> LTL a -> LTL a
weakRelease p = \q -> LTL $ \el _weak -> case el of
  Nothing -> Success
  Just _ -> step (and q (or p (next (weakRelease p q)))) el Weak
{-# INLINEABLE weakRelease #-}

-- | Logical implication: p → q
implies :: LTL a -> LTL a -> LTL a
implies = or . neg
{-# INLINE implies #-}

-- | Eventually the formula will hold, typically written F p or ◇ p.
eventually :: LTL a -> LTL a
eventually = until top
{-# INLINE eventually #-}

-- | Eventually the formula will hold, typically written F p or ◇ p.
weakEventually :: LTL a -> LTL a
weakEventually = weakUntil top
{-# INLINE weakEventually #-}

-- | Always the formula must hold, typically written G p or □ p.
always :: LTL a -> LTL a
always = release (bottom "always")
{-# INLINE always #-}

-- | Always the formula must hold, typically written G p or □ p.
weakAlways :: LTL a -> LTL a
weakAlways = weakRelease (bottom "weakAlways")
{-# INLINE weakAlways #-}

-- | True if the given Haskell boolean is true.
truth :: Bool -> LTL a
truth True = top
truth False = bottom "truth"
{-# INLINE truth #-}

-- | True if the given predicate on the input is true.
is :: (a -> Bool) -> LTL a
is = examine . (truth .)
{-# INLINE is #-}

-- | Another name for 'is'.
test :: (a -> Bool) -> LTL a
test = is
{-# INLINE test #-}

-- | Check for equality with the input.
eq :: Eq a => a -> LTL a
eq = is . (==)
{-# INLINE eq #-}
