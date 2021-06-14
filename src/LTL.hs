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
  ( LTL
  , Machine(..)
  , Reason(..)
  , Result
  , run

  , neg
  , top
  , bottom
  , examine
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

  , showResult
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
  deriving (Show, Generic, NFData, Functor)

type Result a = Maybe (Reason a)

type LTL a = Machine a (Result a)

-- | ⊤, or "true"
top :: LTL a
top = stop Nothing
{-# INLINE top #-}

-- | ⊥, or "false"
bottom :: String -> LTL a
bottom = stop . Just . HitBottom
{-# INLINE bottom #-}

-- | Negate a formula: ¬ p
neg :: LTL a -> LTL a
neg = fmap invert
{-# INLINE neg #-}

-- | Boolean conjunction: ∧
and :: LTL a -> LTL a -> LTL a
and (Machine f) g = Machine $ \a -> case f a of
  Right (Just e) -> Right (Just (LeftFailed e))
  Right Nothing  -> step g a
  Left f'        -> case step g a of
    Right (Just e) -> Right (Just (RightFailed e))
    Right Nothing  -> Left f'
    Left g'        -> Left $! f' `and` g'

andNext :: LTL a -> LTL a -> LTL a
andNext (Machine f) g = Machine $ \a -> case f a of
  Right (Just e) -> Right (Just (LeftFailed e))
  Right Nothing  -> Left g
  Left f'        -> Left $! f' `and` g
{-# INLINE andNext #-}

-- | Boolean disjunction: ∨
or :: LTL a -> LTL a -> LTL a
or (Machine f) g = Machine $ \a -> case f a of
  Right Nothing   -> Right Nothing
  Right (Just e1) -> case step g a of
    Right (Just e2) -> Right (Just (BothFailed e1 e2))
    g'              -> g'
  Left f' -> case step g a of
    Right Nothing  -> Right Nothing
    Right (Just _) -> Left f'
    Left g'        -> Left $! f' `or` g'

orNext :: LTL a -> LTL a -> LTL a
orNext (Machine f) g = Machine $ \a -> case f a of
  Right Nothing  -> Right Nothing
  Right (Just _) -> Left g
  Left f'        -> Left $! f' `or` g
{-# INLINE orNext #-}

invert :: Result a -> Result a
invert = \case
  Nothing -> Just (HitBottom "neg")
  Just _  -> Nothing
{-# INLINE invert #-}

stop :: Result a -> LTL a
stop = Machine . const . Right
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
examine f = Machine $ \a -> step (f a) a
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
next = Machine . const . Left
{-# INLINE next #-}

-- | The "until" temporal modality, typically written 'p U q'.
until :: LTL a -> LTL a -> LTL a
until p = \q -> fix $ or q . andNext p
{-# INLINE until #-}

-- | Weak until.
weakUntil :: LTL a -> LTL a -> LTL a
weakUntil p = \q -> (p `until` q) `or` always p
{-# INLINE weakUntil #-}

-- | Release, the dual of 'until'.
release :: LTL a -> LTL a -> LTL a
release p = \q -> fix $ and q . orNext p
{-# INLINE release #-}

-- | Strong release.
strongRelease :: LTL a -> LTL a -> LTL a
strongRelease p = \q -> (p `release` q) `and` eventually p
{-# INLINE strongRelease #-}

-- | Logical implication: p → q
implies :: LTL a -> LTL a -> LTL a
implies = or . neg
{-# INLINE implies #-}

-- | Eventually the formula will hold, typically written F p or ◇ p.
eventually :: LTL a -> LTL a
eventually = until top
{-# INLINE eventually #-}

-- | Always the formula must hold, typically written G p or □ p.
always :: LTL a -> LTL a
always = release (bottom "always")
{-# INLINE always #-}

-- | True if the given Haskell boolean is true.
truth :: Bool -> LTL a
truth True  = top
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

-- | Render a 'step' result as a string.
showResult :: Show a => Either (LTL a) (Result a) -> String
showResult (Left _)  = "<need input>"
showResult (Right b) = show b
{-# INLINE showResult #-}
