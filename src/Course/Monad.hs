{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
--                  -- or --
--   `∀f g x. (x >>= f) >>= g ≅ x >>= (f . (>>= g))`
class Applicative k => Monad k where
  -- Pronounced, bind.
  (=<<) ::
    (a -> k b)
    -> k a
    -> k b

infixr 1 =<< -- right associative, with low precendence

-- There are actually two operators in the Prelude (both are called bind,
-- they are identical by a flip -- that is direction because of sequencing)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b  <-- bind, flipped
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b  <-- bind

-- >>= : Monad sequencing operator with value passing
-- >> : Monad sequencing operator (without value passing)

-- m >>= k = k =<< m = join (k <$> m) = join (fmap k m)


-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> ExactlyOne b)
    -> ExactlyOne a
    -> ExactlyOne b
  (=<<) =
    bindExactlyOne

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) =
    flatMap

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) =
    bindOptional

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) ::
    (a -> ((->) t b))  -- (a -> (t -> b))
    -> ((->) t a)      -- -> (t -> a)
    -> ((->) t b)      -- -> (t -> b)
  (=<<) g m t =
    g (m t) t -- NOTE: We are using t (the environment) twice!!!

-- Used for configuration in haskell
-- Reader
-- ReaderT <- This (monad transformer) is the more useful
--
-- data Config = { appName :: String }
-- ReaderT Config IO

-- Used like this: Go off and read the config and print the appName
-- asks AppName >>= print

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
(<**>) ::
  Monad k =>
  k (a -> b)
  -> k a
  -> k b
(<**>) mf mx =
  (<$> mx) =<< mf
  -- equivalent to this less point free expression
  -- (\f -> f <$> mx) =<< mf
  -- mf >>= (\f -> f <$> mx)
  -- mf >>= (<$> mx) <-- Using bind, pipe is more idiomatic I think
  -- So you basicly bind the lifted function and map the monadic value over the unwrapped function

-- (<$>) ::   (a -> b) -> k a -> k b    <-- covariant functor (aka Functor)
-- (<*>) :: k (a -> b) -> k a -> k b    <-- applicative functor (aka Applicative)
-- (=<<) :: (a -> k b) -> k a -> k b    <-- monadic functor (aka Monad)


-- This is wrong
  -- (\x -> (\f -> f x) <$> mf) =<< mx -- NOTE: (\f -> f x) == ($ x)
  --(\x -> ($ x) <$> mf) =<< mx -- TODO: List test fejler!!!

-- EXPECTED: [2,3,4,2,4,6]
-- ((+1) :. (*2) :. Nil) <**> (1 :. 2 :. 3 :. Nil)
-- ( \x -> ($ x) <$> ((+1) :. (*2) :. Nil) ) =<< (1 :. 2 :. 3 :. Nil)
-- PROBLEM: 1 from RHS is mapped over (+1) and (*2), We want instead that
--          (+1) is mapped over [1,2,3], and then (*2) is mapped over [1,2,3]
--
-- (+1) <$> (1 :. 2 :. 3 :. Nil) -> [2,3,4]
-- (*2) <$> (1 :. 2 :. 3 :. Nil) -> [2,4,6]

-- Thats more like it!!!
-- \f -> f <$> mx =<< mf
-- ( \f -> f <$> (1 :. 2 :. 3 :. Nil) ) =<< ((+1) :. (*2) :. Nil)

infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Monad k =>
  k (k a)
  -> k a
join =
  -- bind will unwrap and combine everything because we are using identity target
  --id =<< mmx
  (id =<<)


-- This is a bit weird (reader monad)
-- join (+) 7
-- (id =<< (+)) 7
-- (id =<< \x y -> x + y) 7 <-- reader monad
-- (id =<< (\x -> (\y -> x + y))) 7
-- (id =<< (\x -> (\y -> x + y))) 7 <-- reader inside reader
-- ((\x -> (\y -> x + y)) 7) 7 <-- reduce bu reader bind here
-- (\y -> 7 + y) 7
-- 14

-- NOTE: reader bind
-- (=<<) g mx t = g (mx t) t


-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Monad k =>
  k a
  -> (a -> k b)
  -> k b
(>>=) = flip (=<<)
-- Equivalent to
-- (>>=) g mx =
--   mx =<< g

-- reader monad again...
--     (+10) is a reader
--     (*) is a cross-world function x -> reader x
-- ((+10) >>= (*)) 7
-- ((\x -> x + 10) >>= (\x y -> x * y)) 7

-- (\x y -> x * y)) ((\x -> x + 10) 7) 7
-- (\x y -> x * y)) 17) 7
-- (\y -> 17 * y)) 7
-- (\y -> 17 * y)) 7
-- 17 * 7
-- 119

-- NOTE: reader bind, flipped
-- (>>=) mx g t = g (mx t) t

infixl 1 >>= -- left associative, with low precedence 1

-- | Implement composition within the @Monad@ environment.
-- Pronounced, Kleisli composition. aka "fish operator"
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Monad k =>
  (b -> k c)        --    (b -> k c)
  -> (a -> k b)     -- -> (a -> k b)
  -> a              -- -> (a -> k c)
  -> k c
(<=<) f g x =
  f =<< g x

-- NOTE: Kleisli is ReaderT -- you can wrap monadic function (a -> k b) in a data type

-- NOTE how compose is defined
--   (.) :: (b ->   c) -> (a ->   b) -> (a ->   c)
-- (<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)

-- NOTE: Monad laws can be specified elegantly in terms of the kleisli (fish) operator
--
--    g =<< (f =<< mx) ≅ ((g =<<) . f) =<< mx

-- TODO

-- ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- (\n -> n :. n :. Nil) =<< (\n -> n+1 :. n+2 :. Nil) 1
-- (\n -> n :. n :. Nil) =<< (2 :. 3 :. Nil)
-- flatMap (\n -> n :. n :. Nil) (2 :. 3 :. Nil)
-- (flatten . map (\n -> n :. n :. Nil)) (2 :. 3 :. Nil)
-- flatten $ map (\n -> n :. n :. Nil) (2 :. 3 :. Nil)

-- NOTE: list bind is cartesian product, where from part is applied one at a time

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
