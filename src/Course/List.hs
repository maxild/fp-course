{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo: ...") with an appropriate
--   solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import Course.Optional
import qualified System.Environment as E
import qualified Prelude as P
import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap ((P.foldr (:.) Nil) :: ([a] -> List a)) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
-- NOTE: This is aclled a cons-list (a singly-linked list in C#)
data List t =
  Nil
-- | (:.) t (List t) <-- This is the quivalent non-infix (prefix) definition
  | t :. List t
  deriving (Eq, Ord)

-- Right-associative: 0 .: 1 .: 2 .: Nil   is really   0 .: (1 .: (2 .: Nil))
infixr 5 :.

instance Show t => Show (List t) where
  -- convert to a prelude list
  show = show . hlist

-- The list of integers from zero to infinity.
infinity :: List Integer
infinity =
  -- recursive local function that loops/recurses forever (lazy evaluation in Haskell)
  let inf x = x :. inf (x+1)
  in inf 0

--
-- functions over List that you may consider using
--

------------------------------------------------------------
-- NOTE: We are not folding from the right, we are doing
--          CONSTRUCTOR REPLACEMENT == foldRight
------------------------------------------------------------

--   ( ~ right associative operator semantics, just like cons (:.) and Nil )

-- foldRight cons-replacement nil-replacement
-- foldRight (+) 0 (0 :. 1 :. 2 :. Nil)

-- (0 :. 1 :. 2 :. Nil)
-- (0 + (1 + (2 + 0)))   <-- constructor replacement for right-assoc operator

-- foldRight f a (0 :. 1 :. 2 :. Nil)
-- 0 `f` (1 `f` (2 `f` a))
-- f 0 (f 1 (f 2 a))

-- Catamorphism = Constructor Replacement
-- foldr: traversing and evaluating a recursive data structure like list
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- `f` = # is some operator that is right-associative
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ acc Nil      = acc
foldRight f acc (h :. t) = f h (foldRight f acc t)

-- NOTE about TAIL RECURSION in Haskell!!!

-- > With that said, tail recursion is not that useful of a concept in a
--   lazy language like Haskell.  The important concept to know in Haskell
--   is 'guarded recursion', where any recursive calls occur within a data
--   constructor (such as foldr, where the recursive call to foldr occurs
--   as an argument to (:)).  This allows the result of the function to be
--   consumed lazily, since it can be evaluated up to the data constructor
--   and the recursive call delayed until needed.

-- Example using Prelude "syntax"
--     foldr (+) 0 [1,2,3]
--  -> 1 + foldr (+) 0 [2,3]
-- At this point, the top level (+) is going to pattern match on both its arguments.
-- 1 is already evaluated, so there's nothing to do, while foldr (+) 0 [2,3] is not,
-- so the pattern match inside (+) will wait on the stack while we evaluate
-- foldr (+) 0 [2,3].
--  -> 1 + (2 + foldr (+) 0 [3])
--  -> 1 + (2 + (3 + foldr (+) 0 []))
--  -> 1 + (2 + (3 + 0)) <- The stack has 3 stack frames here!!!

-- foldLeft f (f acc h) t

-- let acc' = f acc h in
--            acc' `seq` foldLeft f acc' t

-- Compare this to foldl (without `seq`, foldl' has seq)
--     foldl (+) 0 [1,2,3]
--  -> foldl (+) (0 + 1) [2,3]
--  -> foldl (+) ((0 + 1) + 2) [3]
--  -> foldl (+) (((0 + 1) + 2) + 3) []
--  -> ((0 + 1) + 2) + 3
-- 1. Up to this point, the computation will have used essentially no stack space,
-- since the only pattern matches going on have been against lists that are already
-- in an evaluated state.
-- 2. But now we have built up a big expression involving lots of (+)'s and we know
-- that it needs to pattern match both its arguments. So the outermost (...) + 3
-- will end up waiting on the stack for ((0+1)+2) to evaluate, and so on. If the
-- list was long to begin with, we'll again end up using lots of stack space.
-- 3. To fix this, we need to make sure that the foldl eagerly evaluates its 'acc'
-- argument on each step, so we don't build up the big expression in the first place.
-- I've been assuming lazy evaluation here, but GHC, at least with optimisations
-- turned on, is actually smarter than a plain lazy evaluator. It will figure out
-- that it's better to make the foldl evaluate its argument as it goes (since it
-- figures out that it's going to immediately need the result after, so there's no
-- harm in it). Effectively, this turns plain foldl into an eagerly evaluated
-- foldl in almost all the cases where it would be beneficial. If you're scared that
-- this optimisation might not work, or need to run non-optimised code on large inputs,
-- there's a variant called foldl' which explicitly forces the evaluation:
--    foldl' f acc [] = acc
--    foldl' f acc (x:xs) = let acc' = f acc x in seq acc' (foldl' f acc' xs)
-- This is no longer tail recursive by some definitions, but seq is a primitive which,
-- if the evaluation of x doesn't terminate, seq x acc' doesn't terminate, but otherwise,
-- it's equal to acc'. This is enough to hint to the compiler that it can evaluate the
-- let-bound acc' before recursing in the above.

-- IMPORTANT:
-- ==========
-- So should you avoid foldr?
-- No! In fact, foldr is perhaps more often the correct thing to use in Haskell
-- than foldl' is, but it depends on what you're doing with the elements of the list.
-- If you're applying a function that can't produce any of its result without pattern
-- matching both its arguments, then definitely you want foldl'. If your combining
-- function can produce some part of its result without needing to look at both its
-- arguments, then foldr can short circuit.
-- That is: foldr immediately passes control to f, and only if f needs to look at
-- its second argument will the foldr continue.

-- If you're doing something like filter:
--     filter p = foldr (\x xs -> if p x then x:xs else xs) []
-- or find:
--     find p = foldr (\x m -> if p x then Just x else m) Nothing
-- this will work on infinite or very long lists, and short circuit (in filter's case,
-- it will only use as much of the input as needed to produce the output you asked for).
-- By contrast, foldl on an infinite list just applies foldl to more and more complicated
-- arguments forever, looking for the end of the list.
--
-- if you're writing concat:
--    concat = foldr (++) []
-- This is advantageous for a number of reasons -- you can begin to get output from
-- the first list before looking at the rest and it works on infinite lists like above,
-- but also xs ++ ys takes O(length xs) time to compute, so if you'd written a foldl,
-- you'd be running into a quadratic time worst (and average) case.



-- See hlist below (for pretty printing)
--    foldRight (:) [] (0 :. 1 :. 2 :. Nil)
-- -> 0 : foldRight (:) [] (1 :. 2 :. Nil)
-- -> 0 : 1 : foldRight (:) [] (2 :. Nil)
-- -> 0 : 1 : 2 : foldRight (:) [] Nil
-- -> 0 : 1 : 2 : []
-- -> [0,1,2]

-- foldRight const 3 (0 :. 1 :. 2 :. 3 .: Nil)
-- 0 `const` 1 `const` 2 `const` 3 `const` 3
-- const 0 (const 1 (const 2 (const 3 3)))
-- BUT const = \x _ -> x
-- THEREFORE: const 0 ? evaluates to 0 when outermost redex is reduced

-- NOTE: If replaced cons (f) can be evaluated without 'looping/recursing' over
--       the entire list we can work with infinite list.
-- headOr 3 infinity
-- foldRight const 3 infinity
-- NOTE: The deconstructing pattern matching in foldRight will drive the infinity reductions
-- foldRight const 3 (0 :. inf 1)
-- const 0 (foldRight const 3 (inf 1))
-- 0

-- NOTE: This module overwrites ++ and other List operators, we therefore use

singleton :: t -> List t
singleton x = x :. Nil

intToString :: Int -> List Char
intToString x = singleton (chr (ord '0' + x))

-- xxx :: List Char
-- xxx = foldRight
--         (\x acc -> "(" ++ intToString x ++ " # " ++ acc ++ ")")
--         "v"
--         (0 :. 1 :. 2 :. Nil)
-- "(0 # (1 # (2 # v)))"

------------------------------------------------------------
-- NOTE: We are not folding from the left, we are doing
--          FOR LOOP == foldLeft    (This is not as important as foldRight)
------------------------------------------------------------

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- (((v # x[0]) # x[1]) # x[2]) # x[3]
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ acc Nil      = acc
foldLeft f acc (h :. t) = let acc' = acc `f` h in acc' `seq` foldLeft f acc' t

-- FOR LOOP     ( ~ left associative operator semantics )
-- foldLeft f a xs

-- // Think about it like this in C#
-- var accum = a;
-- foreach (var x in xs)
--    accum = f(accum, x);
-- return accum;

-- NOTE: we have to use append (++) because we are folding left to right (appending)
-- foldLeft (\acc x -> acc ++ singleton x) Nil (0 :. 1 :. 2 :. Nil)

-- `f` = # is some operator that is left-associative
-- yyy :: List Char
-- yyy = foldLeft
--         (\acc x -> "(" ++ acc ++ " # " ++ intToString x ++ ")")
--         "v"
--         (0 :. 1 :. 2 :. Nil)
-- "(((v # 0) # 1) # 2)"

-- END Helper functions and data types

-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> \x -> x `headOr` infinity == 0
--
-- prop> \x -> x `headOr` Nil == x
headOr ::
  a
  -> List a
  -> a
-- headOr = foldRight (\elem acc -> elem)
-- headOr = foldRight (\x _ -> x)
-- const :: a -> b -> a, where const x is unary function that evaluates to x for all input
headOr = foldRight const

-- This is weird, but Haskel is evaluated in a special (no-strict) unique way
-- Q: In what order should redex'es be reduced?
--    1. Innermost evaluation (aka call-by-value evalution)
--    2. Outermost evaluation (aka call-by-name evaluetion, because arguments are not
--    evaluated before the outermost function expression needs them to be)
-- Haskell uses call-by-name evaluation (aka lazy evaluation). This will make it
-- possible to program with infinite structures.

-- Check out the reductions performed by GHC:
--    headOr 4 infinity
--    foldRight const 4 infinity  <-- applying headOr (not infinity!!!!)
--    const 0 (foldRight const 4 (1 :. infinity...)) <-- applying foldRight
--    0    <-- applying const, because of call-by-name (Outermost) evaluation
-- Note: because const just returns its first argument and haskell is lazy we are done

-- ctor replacement
-- foldRight (\???) a Nil
-- => a

-- foldRight (\???) a (1 :. 2 :. Nil)
-- => (1 `f` 2 `f` a)
-- => f 1 (f 2 a)

-- Pattern Matching Solution
-- headOr x Nil      = x
-- headOr _ (h :. _) = h

-- | The product of the elements of a list.
--
-- >>> product Nil
-- 1
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
product ::
  List Int
  -> Int
product = foldRight (*) 1

-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> \x -> foldLeft (-) (sum x) x == 0
sum ::
  List Int
  -> Int
sum = foldRight (+) 0

-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> \x -> sum (map (const 1) x) == length x
length ::
  List a
  -> Int
-- length = foldRight (\_ len -> len + 1) 0
-- length = foldRight (const (1+)) 0
length = foldRight (const (+1)) 0 -- I don't think this is more readable!!!

-- Parametricity: By the signature 'List a -> Int' we can tell that the elements
--                are not used, only the structure of the list.

-- (+1) :: Num a => a -> a
-- const (+1) :: Num a => b -> a -> a, where the function returns (+1) for all elements

-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> \x -> headOr x (map (+1) infinity) == 1
--
-- prop> \x -> map id x == x
map :: (a -> b) -> List a -> List b
-- map f = foldRight (\x ys -> f x :. ys) Nil --  <-- The most readable, I think
-- map f = foldRight (\x -> (f x :.)) Nil
-- map f = foldRight (\x -> (:.) (f x)) Nil
-- Using lambdabot (pl == pointfree)
map f = foldRight ((:.) . f) Nil

-- We deconstructing the list, and then constructing it back up

-- recursive defintion
-- map _ Nil       = Nil
-- map f (x :. xs) = f x :. map f xs

-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> \x -> headOr x (filter (const True) infinity) == 0
--
-- prop> \x -> filter (const True) x == x
--
-- prop> \x -> filter (const False) x == Nil
filter ::
  (a -> Bool)
  -> List a
  -> List a
filter p = foldRight (\x ys -> if p x
                               then x :. ys
                               else ys) Nil

-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> \x -> headOr x (Nil ++ infinity) == 0
--
-- prop> \x -> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> \x -> x ++ Nil == x
(++) ::
  List a
  -> List a
  -> List a
-- xs ++ ys = foldRight (:.) ys xs -- loop backwards with foldRight, because cons cannot append to the end
-- (++) xs ys = foldRight (:.) ys xs  <-- I think this is the most readable!!!

-- foldRight (:.) :: List t -> List -> List t, but the lists are in the wrong order
-- flip will reverse the args of it argument function
(++) = flip (foldRight (:.))

-- We are doing ctor replacement on the first list, where
--      Nil is repleced with second list
--      :. is not replaced (identity)

infixr 5 ++

-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> sum (map length x) == length (flatten x)
flatten ::
  List (List a)
  -> List a
flatten = foldRight (++) Nil

-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> flatMap id (x :: List (List Int)) == flatten x  <-- THAT IS THE ANSWER!!!!
flatMap ::
  (a -> List b)
  -> List a
  -> List b
flatMap f = flatten . map f -- join . lift f (category theory)
-- flatMap f as = flatten (map f as)
-- flatMap f = foldRight (\a bs -> f a ++ bs) Nil
-- flatMap f xs = flatten (map f xs) -- bind = join . map


-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x
flattenAgain ::
  List (List a)
  -> List a
flattenAgain = flatMap id -- We do not cross-world, and therefore we just flatten

-- NOTES (IMPORTANT!!!! when we do Monad!!!!)
--    We can use flatten to implement flatMap (bind)
--    We can use flatMap (bind) to implement flatten

-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values,
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional =
  -- This is sequence from the Prelude t (f a) -> f (t a), where T is Traversable, M is Functor/Applicative/Monad
  -- twiceOptional == lidtA2
  -- foldRight (liftA2 (:.)) (pure Nil)
  -- So we are deconstructing the list, and reconstructing the list with cons and nil that are works with Optional (lifted to the Maybe world)
  foldRight (twiceOptional (:.)) (Full Nil) -- applicative style!!!

  -- foldRight (\opt optList -> case opt of
  --                          Full x -> mapOptional (x :.) optList
  --                          Empty -> Empty) (Full Nil)

-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find ::
  (a -> Bool)
  -> List a
  -> Optional a
-- NOTE: When predicate evalutes to true then second argument of cons function is
--       not used, and foldRight doesn't have to construct ('loop') no more because
--       const funcktion has become 'const' (K combinator)...see last test above
find p = foldRight (\a o -> if p a then Full a else o) Empty

-- This works, but is not as straight forward
-- find p = foldLeft (\o a -> if isEmptyOptional o && p a
--                              then Full a
--                              else o)
--                   Empty

-- This one will loop forever (infinite loop), and tests would break!!
-- find p =
--   foldLeft (\o a -> if p a then Full a else o) Empty

-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 ::
  List a
  -> Bool
-- Trick is to use helper function (drop) implemented using recursion (not foldRight)
lengthGT4 = not . isEmptyList . drop 4

isEmptyList :: List t -> Bool
isEmptyList Nil = True
isEmptyList _   = False

-- f 1 (f 2 nil)

-- NOTE: remember NIL, and we can't put in the pattern
-- lengthGT4 (_ :. _ :. _ :. _ :. _ :. _) = True
-- lengthGT4 _                            = False

-- This traverses the hole list...NOT good => infinite recursion in the tests!!!
-- lengthGT4 xs = length xs > 4 -- BUG!!!!

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse ::
  List a
  -> List a
reverse = foldLeft (flip (:.)) Nil

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce f x = x :. produce f (f x) -- iterate from the Prelude

-- produce (+1) 0
-- 0 :. produce (+1) ((+1) 0) = 0 :. produce (+1) 1
-- 0 :. 1 :. produce (+1) ((+1) 1) = 0 :. 1 :. produce (+1) 2
-- etc...

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> \x y -> let types = x :: List Int
--               in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> \x -> let types = x :: Int
--             in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse = reverse -- NOTE: Types (parametricity) and tests (specifications) are important!!!

---- End of list exercises

largeList ::
  List Int
largeList =
  listh [1..50000]

hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

listh ::
  [a]
  -> List a
listh =
  P.foldr (:.) Nil

putStr ::
  Chars
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  Chars
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  FilePath
  -> IO Chars
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  FilePath
  -> Chars
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

getLine ::
  IO Chars
getLine =
  P.fmap listh P.getLine

getArgs ::
  IO (List Chars)
getArgs =
  P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf ::
  Eq a =>
  List a
  -> List a
  -> Bool
isPrefixOf Nil _ =
  True
isPrefixOf _  Nil =
  False
isPrefixOf (x:.xs) (y:.ys) =
  x == y && isPrefixOf xs ys

isEmpty ::
  List a
  -> Bool
isEmpty Nil =
  True
isEmpty (_:._) =
  False

span ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
span p x =
  (takeWhile p x, dropWhile p x)

break ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
break p =
  span (not . p)

dropWhile ::
  (a -> Bool)
  -> List a
  -> List a
dropWhile _ Nil =
  Nil
dropWhile p xs@(x:.xs') =
  if p x
    then
      dropWhile p xs'
    else
      xs

takeWhile ::
  (a -> Bool)
  -> List a
  -> List a
takeWhile _ Nil =
  Nil
takeWhile p (x:.xs) =
  if p x
    then
      x :. takeWhile p xs
    else
      Nil

zip ::
  List a
  -> List b
  -> List (a, b)
zip =
  zipWith (,)

zipWith ::
  (a -> b -> c)
  -> List a
  -> List b
  -> List c
zipWith f (a:.as) (b:.bs) =
  f a b :. zipWith f as bs
zipWith _ _  _ =
  Nil

unfoldr ::
  (a -> Optional (b, a))
  -> a
  -> List b
unfoldr f b  =
  case f b of
    Full (a, z) -> a :. unfoldr f z
    Empty -> Nil

lines ::
  Chars
  -> List Chars
lines =
  listh . P.fmap listh . P.lines . hlist

unlines ::
  List Chars
  -> Chars
unlines =
  listh . P.unlines . hlist . map hlist

words ::
  Chars
  -> List Chars
words =
  listh . P.fmap listh . P.words . hlist

unwords ::
  List Chars
  -> Chars
unwords =
  listh . P.unwords . hlist . map hlist

listOptional ::
  (a -> Optional b)
  -> List a
  -> List b
listOptional _ Nil =
  Nil
listOptional f (h:.t) =
  let r = listOptional f t
  in case f h of
       Empty -> r
       Full q -> q :. r

any ::
  (a -> Bool)
  -> List a
  -> Bool
any p =
  foldRight ((||) . p) False

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight ((&&) . p) True

or ::
  List Bool
  -> Bool
or =
  any id

and ::
  List Bool
  -> Bool
and =
  all id

elem ::
  Eq a =>
  a
  -> List a
  -> Bool
elem x =
  any (== x)

notElem ::
  Eq a =>
  a
  -> List a
  -> Bool
notElem x =
  all (/= x)

permutations
  :: List a -> List (List a)
permutations xs0 =
  let perms Nil _ =
        Nil
      perms (t:.ts) is =
        let interleave' _ Nil r =
              (ts, r)
            interleave' f (y:.ys) r =
               let (us,zs) = interleave' (f . (y:.)) ys r
               in  (y:.us, f (t:.y:.us):.zs)
        in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
  in xs0 :. perms xs0 Nil

intersectBy ::
  (a -> b -> Bool)
  -> List a
  -> List b
  -> List a
intersectBy e xs ys =
  filter (\x -> any (e x) ys) xs

take ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
take n _  | n <= 0 =
  Nil
take _ Nil =
  Nil
take n (x:.xs) =
  x :. take (n - 1) xs

drop ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
drop n xs | n <= 0 =
  xs
drop _ Nil =
  Nil
drop n (_:.xs) =
  drop (n-1) xs

repeat ::
  a
  -> List a
repeat x =
  x :. repeat x

replicate ::
  (Num n, Ord n) =>
  n
  -> a
  -> List a
replicate n x =
  take n (repeat x)

reads ::
  P.Read a =>
  Chars
  -> Optional (a, Chars)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Chars
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Chars
  -> Optional (a, Chars)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Chars
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Chars
  -> Optional (a, Chars)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Chars
  -> Optional a
readFloat =
  mapOptional fst . readFloats

-- string literals will become List Char
instance IsString (List Char) where
  fromString =
    listh

-- {Char] == String from Prelude
type Chars =
  List Char

type FilePath =
  List Char

strconcat ::
  [Chars]
  -> P.String
strconcat =
  P.concatMap hlist

stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

show' ::
  Show a =>
  a
  -> List Char
show' =
  listh . show

instance P.Functor List where
  fmap f =
    listh . P.fmap f . hlist -- TODO: why not just: fmap = map

instance A.Applicative List where
  (<*>) =
    M.ap
  pure =
    (:. Nil)

instance P.Monad List where
  (>>=) =
    flip flatMap
  return =
    (:. Nil)
