-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE RecordWildCards #-}

module Monoids where

import Data.Foldable
import Data.Semigroup


-- | Associative binary functions.  Values of this type should satisfy
-- the following law:
--
--   * Associativity: for all x, y, z,
--     @(x `sappend` y) `sappend` z = x `sappend` (y `sappend` z)@.

newtype ASemigroup a =
    ASemigroup {
      sappend :: a -> a -> a
    }

sAdd :: ASemigroup Integer
sAdd = ASemigroup (+)

sMult :: ASemigroup Integer
sMult = ASemigroup (*)

sAppend :: ASemigroup [a]
sAppend = ASemigroup (++)

sAndThen :: ASemigroup (IO a)
sAndThen = ASemigroup (>>)

sBad :: ASemigroup Integer
sBad = ASemigroup (-)

sAddExample :: Integer
sAddExample =
    let ASemigroup{..} = sAdd
    in 3 `sappend` 4 `sappend` 5 `sappend` 6

sAppendExample :: String
sAppendExample =
    let ASemigroup{..} = sAppend
    in foldr sappend [] ["A", "List", "Of", "Words"]

stimes' :: ASemigroup a -> Integer -> a -> a
stimes' sg@ASemigroup{..} n x =
    case compare n 1 of
      LT -> error "stimes': non-positive count"
      EQ -> x
      GT -> x `sappend` stimes' sg (n - 1) x

sUnit :: ASemigroup ()
sUnit = ASemigroup (\_ _ -> ())

exerciseSg2a :: ASemigroup (IO a)
exerciseSg2a = ASemigroup (\c1 c2 -> c2 >> c1)

exerciseSg2b :: ASemigroup Rational
exerciseSg2b = ASemigroup (/)

exerciseSg2c :: ASemigroup (Integer, Integer)
exerciseSg2c = ASemigroup (\(x1, x2) (y1, y2) -> (x1 + y2, x2 + y1))

exerciseSg2d :: ASemigroup Integer
exerciseSg2d = ASemigroup max

exerciseSg2e :: ASemigroup Integer
exerciseSg2e = ASemigroup (\x y -> x * (-y))

exerciseSg2f :: ASemigroup [a]
exerciseSg2f = ASemigroup f
    where
    f xs     []     = xs
    f []     ys     = ys
    f (x:xs) (y:ys) = x : y : f xs ys

exerciseSg3a :: ASemigroup (a -> a)
exerciseSg3a = ASemigroup (.)

exerciseSg3b :: ASemigroup (a -> Integer)
exerciseSg3b = ASemigroup (\f g x -> f x + g x)

exerciseSg3c :: ASemigroup (a -> IO a)
exerciseSg3c = ASemigroup (\f g x -> f x >>= g)

exerciseSgM1 :: () -> Integer
exerciseSgM1 _ = 0

myStimes :: (Semigroup a) => Integer -> a -> a
myStimes n x =
    case compare n 1 of
      LT -> error "myStimes: Non-positive count"
      EQ -> x
      GT -> x <> myStimes (n - 1) x

newtype MySum a = MySum { getMySum :: a }

instance (Num a) => Semigroup (MySum a) where
    MySum x <> MySum y = MySum (x + y)

newtype MyProduct a = MyProduct { getMyProduct :: a }

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct x <> MyProduct y = MyProduct (x * y)

listLen :: [a] -> Integer
listLen = foldl' (\c _ -> 1 + c) 0

newtype AndThen = AndThen { runAndThen :: IO () }

instance Monoid AndThen where
    mappend (AndThen c1) (AndThen c2) =
        AndThen (c1 >> c2)

    mempty = AndThen (pure ())

newtype MyMax a = MyMax { fromMyMax :: a }

instance (Ord a) => Semigroup (MyMax a) where
    MyMax x <> MyMax y = MyMax (max x y)

instance (Bounded a, Ord a) => Monoid (MyMax a) where
    mappend = (<>)
    mempty  = MyMax minBound
