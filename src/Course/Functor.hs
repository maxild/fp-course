{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`    <=>     `∀x. (fmap id x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`  <=>  `∀f g x.(fmap (f . g) x) ≅ (fmap f (fmap g x))`
-- This is what is called *Covariant Functor* in mathematics (because it doesn't turn around composition)
class Functor k where -- NOTE: Why not use f (not k) for * -> * HKT?
  -- Pronounced, eff-map.
  --   "lift" a (1 -arg) function into functor world (i.e. lifting a function such
  --   that it works over type constructors)
  -- map :: (  a ->   b) ->
  --        (k a -> k b)
  --         -- or --
  -- map f :: apply a function to the content of a functor
  --    (a -> b) -> k a -> k b
  (<$>) ::
    (a -> b)
    -> k a
    -> k b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings   <-
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance Functor ExactlyOne where
  -- Note: This is our own custom Functor class
  -- NOTE: We do not define fmap, and differs from the Prelude
  (<$>) ::
    (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<$>) f =
    -- mapExactlyOne f
    ExactlyOne . f . runExactlyOne

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  (<$>) =
    map

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  (<$>) =
    mapOptional

-- MOTIVATION: The same input is used again and again, and we want to pass it once
-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  -- (<$>) g r = g . r -- composition
  (<$>) = (.)


-- Utility function
-- TODO: Aren't they part of Functor type class?

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) ::
  Functor k =>
  a
  -> k b
  -> k a
(<$) x f =
  const x <$> f


-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor k =>
  k a
  -> k ()
void f =
  () <$ f


-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
