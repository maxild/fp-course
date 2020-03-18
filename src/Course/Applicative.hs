{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative where

import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P(fmap, return, (>>=))

-- | All instances of the `Applicative` type-class must satisfy four laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. pure id <*> x = x`
--
-- * The law of composition
--   `∀u v w. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
--
-- * The law of homomorphism
--   `∀f x. pure f <*> pure x = pure (f x)`
--
-- * The law of interchange
--   `∀u y. u <*> pure y = pure ($ y) <*> u`

--   This is called *Applicative Functor* in mathematics
-- Applicative Functors allow sequencing of 'functorial' computations (unlike plain
-- functors) but without deciding on which computation to perform on the basis of the
-- result of a previous computation (unlike monads). Therefore monadic computations
-- allow more freedom than functorial computations in the (sequencing) pipeline.
class Functor k => Applicative k where
  pure ::
    a -> k a
  (<*>) ::
    k (a -> b)
    -> k a
    -> k b

-- NOTES:      **Combining** Effects (i.e. combining type constructors)
-- The interesting part of applicative functor (that is not part of being only a
-- functor) is that you can combine things. In functor you have only have map
--
--    map :: (a -> b) -> f a -> f b
--
-- and map have only one type constructor in scope. With applicative you also have
-- apply,
--
--    apply :: f (a -> b) -> f a -> f b
--
-- and apply have two type constructors in scope. So for example for Optional/Maybe
-- there are 4 different cases to deal with when pattern patching (deconstructing)
--
--    Full, Full
--    Full, Empty
--    Empty, Full
--    Empty, Empty
--
-- For list there are an infinite number of cases. So apply have a 'combine' dimension
-- just like a Monoid (different laws). And apply can not only short-circuit (Empty)
-- but also combine two effects in multiple ways. That is functor is unique, while
-- there is always at least two different applicative implementations (normal, flipped)
-- and possibly many more (think list, cartesion product vs zip list).

-- NOTES: 3 different ways to specify applicative
--    1. pure and apply                             (Type A)
--    2. pure and lift2                             (Type B)
--    3. pure and tupled (MergeSource in F# 5)      (Type C)
--
-- lift2 ::
--        (a -> b -> c)
--        -> fa
--        -> fb
--        -> fc
--
-- tupled ::         <-- This is: lift2 (,) = lift2 (\a b -> (a, b))
--        f a
--        -> f b
--        -> f (a, b)  <-- MergeSources in F# 5

infixl 4 <*>

-- NOTE: We show here that being an Applicative Functor, you can define map
(<$$>) :: Applicative f => (a -> b) -> f a -> f b
(<$$>) g =
  (pure g <*>) -- <-- This little law is used in lift=map, lift2, lift3 etc

-- TODO: It is possible to create zip3, zip4 etc

-- aka zip, merge
tupled :: Applicative f => f a -> f b -> f (a, b)
tupled = lift2 (\a b -> (a, b))

-- <*> defined in terms of tupled and <$>
tupledApply :: Applicative f => f (a -> b) -> f a -> f b
tupledApply f x = (\(f', x') -> f' x') <$> tupled f x

-- Example impl for Optional
tupledOptional :: Optional a -> Optional b -> Optional (a, b)
tupledOptional (Full x) (Full y) = Full (x, y)
tupledOptional _        _        = Empty

tupledOptional' :: Optional a -> Optional b -> Optional (a, b)
tupledOptional' = twiceOptional (,)

-- | Insert into ExactlyOne.
--
-- prop> \x -> pure x == ExactlyOne x
--
-- >>> ExactlyOne (+10) <*> ExactlyOne 8
-- ExactlyOne 18
instance Applicative ExactlyOne where
  pure ::
    a
    -> ExactlyOne a
  pure =
    ExactlyOne
  (<*>) ::
    ExactlyOne (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<*>) g m =
    -- mapExactlyOne (runExactlyOne g) m
    runExactlyOne g <$> m

-- | Insert into a List.
--
-- prop> \x -> pure x == x :. Nil
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Applicative List where
  pure ::
    a
    -> List a
  pure =
    (:. Nil) -- <-- singleton list is a (pure) list with no effects (non-determinism)
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
  (<*>) gs as =
    -- flatMap :: ((a -> b) -> List b) -> List (a -> b) -> List b
    -- NOTE: bind and map can implement apply (a little law)
    flatMap (<$> as) gs -- <-- This is the cartesion product applicative (ziplist is another)

    -- -- loop over g in gs
    -- foldLeft (\bs g ->
    --   -- apply g to as
    --   let gas = foldRight (\a gas' -> g a :. gas') Nil as in
    --   -- append gas to the result list
    --   bs ++ gas
    --   ) Nil gs

  -- This also works...
  -- foldRight (\g bs ->
  --   -- apply g to as
  --   let gas = foldRight (\a gas' -> g a :. gas') Nil as in
  --   gas ++ bs
  --   ) Nil gs


-- | Insert into an Optional.
--
-- prop> \x -> pure x == Full x
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Applicative Optional where
  pure ::
    a
    -> Optional a
  pure =
    Full
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  (<*>) g a =
    -- bind (<$> oa) og
    bindOptional (flip mapOptional a) g
    -- using pattern matching
    -- case (g, a) of
    --   (Full g', Full a') -> Full (g' a')
    --   _ -> Empty


-- | Insert into a constant function.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
--
-- prop> \x y -> pure x y == x
-- This is the *Reader Applicative* (aka Reader Monad)
instance Applicative ((->) t) where
  pure ::
    a
    -> ((->) t a)
  pure =
    const -- NOTE: pure means that lifted function does not depend on the side-effect (i.e. the environment)
  -- m (a -> b) -> m a -> m b
  (<*>) ::
    ((->) t (a -> b))   -- (t -> a -> b) ->
    -> ((->) t a)       -- (t -> a)
    -> ((->) t b)       -- (t -> b)
  (<*>) g m t =
    g t (m t)  -- NOTE: We are using t (the environment) twice!!!
  -- Equivalent to
  --  (<*>) g m = \t -> g t (m t)

--
-- Utility functions
--


-- | Apply a binary function in the environment (example: configuration).
--
-- >>> lift2 (+) (ExactlyOne 7) (ExactlyOne 8)
-- ExactlyOne 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
-- NOTE: lifts a function that takes 2 (pure) args and return 1 (pure) value in the
--       "normal/pure" world into a parallel function working on 2 args wrapped in
--       type constructors that returns 1 value wrapped in type constructor (i.e
--       a function working in some container/context in the "effects world")
lift2 ::
  Applicative k =>
  (a -> b -> c)
  -> k a
  -> k b
  -> k c
lift2 f x y =
  f <$> x <*> y

-- | Apply a ternary function in the environment.
-- /can be written using `lift2` and `(<*>)`./
--
-- >>> lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9)
-- ExactlyOne 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Applicative k =>
  (a -> b -> c -> d)
  -> k a
  -> k b
  -> k c
  -> k d
lift3 f ka kb kc =
  -- see the sequence
  -- Q: Can arguments be applied in any order inside the context? Seems reasonable
  -- let fbc = fabc <$> a in
  -- let fc = fbc <*> b in
  -- fc <*> c
  -- f <$> ka <*> kb <*> kc
  lift2 f ka kb <*> kc

-- | Apply a quaternary function in the environment.
-- /can be written using `lift3` and `(<*>)`./
--
-- >>> lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10)
-- ExactlyOne 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Applicative k =>
  (a -> b -> c -> d -> e)
  -> k a
  -> k b
  -> k c
  -> k d
  -> k e
lift4 f x y z v =
  lift3 f x y z <*> v

-- | Apply a nullary function in the environment.
lift0 ::
  Applicative k =>
  a
  -> k a
lift0 =
  pure

-- | Apply a unary function in the environment.
-- /can be written using `lift0` and `(<*>)`./
--
-- >>> lift1 (+1) (ExactlyOne 2)
-- ExactlyOne 3
--
-- >>> lift1 (+1) Nil
-- []
--
-- >>> lift1 (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
lift1 ::
  Applicative k =>
  (a -> b)
  -> k a
  -> k b
lift1 =
  (<$>)

-- | Apply, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> \a b c x y z -> (a :. b :. c :. Nil) *> (x :. y :. z :. Nil) == (x :. y :. z :. x :. y :. z :. x :. y :. z :. Nil)
--
-- prop> \x y -> Full x *> Full y == Full y
(*>) ::
  Applicative k =>
  k a
  -> k b
  -> k b
(*>) =
  lift2 (flip const)
  -- lift2 (\_ b -> b)
  -- (\_ b -> b) <$> ka <*> kb


-- | Apply, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2]
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> \x y z a b c -> (x :. y :. z :. Nil) <* (a :. b :. c :. Nil) == (x :. x :. x :. y :. y :. y :. z :. z :. z :. Nil)
--
-- prop> \x y -> Full x <* Full y == Full x
(<*) ::
  Applicative k =>
  k b
  -> k a
  -> k b
(<*) =
  lift2 const

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6 <-- BUG
-- [60,8]
sequence ::
  Applicative k =>
  List (k a)
  -> k (List a)
sequence =
  -- we are lifting cons and nil, and doing an identity fold right
  foldRight (lift2 (:.)) (pure Nil)

-- | Replicate an effect a given number of times.
--
-- /Tip:/ Use `Course.List#replicate`.
--
-- >>> replicateA 4 (ExactlyOne "hi")
-- ExactlyOne ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
-- sequence (('a' :. 'b' :. 'c' :. Nil) :. ('a' :. 'b' :. 'c' :. Nil) :. ('a' :. 'b' :. 'c' :. Nil) :. Nil)
-- foldRight (lift2 (:.)) (pure Nil) (('a' :. 'b' :. 'c' :. Nil) :. ('a' :. 'b' :. 'c' :. Nil) :. ('a' :. 'b' :. 'c' :. Nil) :. Nil)
--
-- and because lift2 cause the applicative apply/combine logic to be used we get cartesion product
--
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative k =>
  Int
  -> k a
  -> k (List a)
replicateA n fa =
  -- sequence will deconstruct the list of applicatives and reconstruct it back up
  -- using (:.) lifted to the applicative world (so Empty will dominate optional,
  -- cartesion product for list etc)
  sequence (replicate n fa)

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil)
-- ExactlyOne [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative k =>
  (a -> k Bool)
  -> List a
  -> k (List a)
filtering p =
  -- lift2 (f x) :: k Bool -> k (List a) -> k (List a)
  foldRight (\x fxs -> lift2 (f x) (p x) fxs) (pure Nil)
  where
    -- f :: a -> Bool -> List a -> List a
    f x b xs =
      if b then x :. xs else xs

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

return ::
  Applicative k =>
  a
  -> k a
return =
  pure

fail ::
  Applicative k =>
  Chars
  -> k a
fail =
  error . hlist

(>>) ::
  Applicative k =>
  k a
  -> k b
  -> k b
(>>) =
  (*>)
