{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Core(
  Eq(..)
, Ord(..)
, Show(..)
, Integral(..)
, RealFrac(..)
, Num(..)
, Fractional(..)
, Bool(..)
, Either(..)
, Ordering(..)
, Int
, Integer
, IO
, Rational
, seq
, error
, undefined
, const
, flip
, curry
, uncurry
, id
, otherwise
, (.)
, ($)
, (&&)
, (||)
, not
, even
, odd
, fst
, snd
, getChar
, on
, first
, second
, (&&&)
, (***)
, IsString(..)
, module Data.Char
, ifThenElse
, bool
) where


import Prelude(
    Eq(..)
  , Ord(..)
  , Show(..)
  , Integral(..)
  , RealFrac(..)
  , Num(..)
  , Fractional(..)
  , Bool(..)
  , Either(..)
  , Ordering(..)
  , Char
  , Int
  , Integer
  , IO
  , Rational
  , seq
  , error
  , undefined
  , const
  , flip
  , curry
  , uncurry
  , id
  , otherwise
  , (.)
  , ($)
  , (&&)
  , (||)
  , not
  , even
  , odd
  , fst
  , snd
  )
import Data.String(
  IsString(..)
  )

import System.IO(
    getChar
  )
import Data.Function(
    on
  )
import Control.Arrow(
    first
  , second
  , (&&&)
  , (***)
  )
import Data.Char

ifThenElse ::
  Bool
  -> a
  -> a
  -> a
ifThenElse True t _ =
  t
ifThenElse False _ f =
  f

-- catamorphism (ternary operator in C#) from Data.Bool
bool ::
  a
  -> a
  -> Bool
  -> a
bool f _ False =
  f
bool _ t True =
  t

-- NOTE: import Control.Arrow
-- >>> and <<< defined for function composition

-- NOTE: import Flow (if library is referenced)
-- apply, |> , <|, compose, .>, and <.
-- apply', !> and  <! (strict function application)

-- -- << in F#
-- (<.) :: (b -> c) -> (a -> b) -> a -> c
-- (<.) = (.)

-- -- >> in F#
-- (.>) :: (a -> b) -> (b -> c) -> a -> c
-- (.>) = flip (.)

-- -- From F#
-- (|>) :: a -> (a -> b) -> b
-- (|>) x f = f x

-- -- From F#
-- (<|) :: (a -> b) -> a -> b
-- (<|) = flip (|>) -- identical to $

