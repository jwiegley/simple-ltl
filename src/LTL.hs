{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module LTL where

import GHC.Generics

data LTL a
  = Top
  | Bottom
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
    Bottom      -> "Bottom"
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
  = HitBottom
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
combine f g =
  case (f, g) of
    (Stop (Failure e), _) -> Stop (Failure (LeftFailed e))
    (_, Stop (Failure e)) -> Stop (Failure (RightFailed e))
    (f', Stop Success)    -> f'
    (Stop Success, g')    -> g'
    (Delay f', Delay g')  -> Delay (combine f' g')
    (Delay f', Query g')  -> Query (\a -> combine (Delay f') (g' a))
    (Query f', Delay g')  -> Query (\a -> combine (f' a) (Delay g'))
    (Query f', Query g')  -> Query (\a -> combine (f' a) (g' a))

select :: Machine a (Result a) -> Machine a (Result a)
       -> Machine a (Result a)
select f g =
  case (f, g) of
    (Stop (Failure e1),
     Stop (Failure e2))    -> Stop (Failure (BothFailed e1 e2))
    (Stop Success, _)      -> Stop Success
    (_, Stop Success)      -> Stop Success
    (Stop (Failure _), g') -> g'
    (f', Stop (Failure _)) -> f'
    (Delay f', Delay g')   -> Delay (select f' g')
    (Delay f', Query g')   -> Query (\a -> select (Delay f') (g' a))
    (Query f', Delay g')   -> Query (\a -> select (f' a) (Delay g'))
    (Query f', Query g')   -> Query (\a -> select (f' a) (g' a))

step :: Machine a b -> a -> Machine a b
step m x = case m of
  Stop r  -> Stop r
  Delay n -> n
  Query f -> step (f x) x

run :: Machine a b -> [a] -> Machine a b
run m l =
  case l of
    []     -> m
    x : xs -> run (step m x) xs

expand :: LTL a -> LTL a
expand l =
  case l of
    Top         -> l
    Bottom      -> l
    Accept v    -> Accept (expand . v)
    Reject v    -> Reject (expand . v)
    And p q     -> And (expand p) (expand q)
    Or p q      -> Or (expand p) (expand q)
    Next p      -> Next (expand p)
    Until p q   -> Or (expand q) (And (expand p) (Next (Until p q)))
    Release p q -> And (expand q) (Or (expand p) (Next (Release p q)))

compile :: LTL a -> Machine a (Result a)
compile l = compile' (expand l)
  where
  compile' = \case
    Top      -> Stop Success
    Bottom   -> Stop (Failure HitBottom)

    Accept v -> Query (compile' . expand . v)
    Reject v -> Query $ \x ->
      fmap (\case Failure _ -> Success
                  Success   -> Failure (Rejected x))
           (compile' (expand (v x)))

    And p q  -> combine (compile' p) (compile' q)
    Or p q   -> select (compile' p) (compile' q)

    Next p   -> Delay (compile' (expand p))

    Until _ _   -> error "never called due to expand"
    Release _ _ -> error "never called due to expand"

negated :: LTL a -> LTL a
negated l =
  case l of
    Top         -> Bottom
    Bottom      -> Top
    Accept v    -> Reject v
    Reject v    -> Accept v
    And p q     -> Or (negated p) (negated q)
    Or p q      -> And (negated p) (negated q)
    Next p      -> Next (negated p)
    Until p q   -> Release (negated p) (negated q)
    Release p q -> Until (negated p) (negated q)

implies :: LTL a -> LTL a -> LTL a
implies p q = Or (negated p) q

eventually :: LTL a -> LTL a
eventually p = Until Top p

always :: LTL a -> LTL a
always p = Release Bottom p

truth :: Bool -> LTL a
truth b = if b then Top else Bottom

eq :: Eq a => a -> LTL a
eq n = Accept $ \x -> truth (x == n)

test :: Machine Int (Result Int)
test = run (compile (implies (eq 2) (eventually (eq 4))))
           [1, 2, 3, 4]
