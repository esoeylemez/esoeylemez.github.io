---
title:     "The Monoid Tutorial (Draft)"
author:    "ertes"
date:      "2016-10-05"
lang:      "en"
copyright: "Ertugrul Söylemez"
want-toc:  "yes"
...

This tutorial is not only educational material for you, but it's also a
playing field for me to explore just how far simple algebraic structures
like monoids can be applied in the context of software design.

[![Creative Commons Attribution-ShareAlike 4.0 International License](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)][cc-by-sa]
This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/


Semigroups
==========

Let's build up to monoids first; don't worry, it's a relatively short
build-up.  A *semigroup* is an associative binary function:

``` haskell
-- | Associative binary functions.  Values of this type should satisfy
-- the following law:
--
--   * Associativity: for all x, y, z,
--     @(x `sappend` y) `sappend` z = x `sappend` (y `sappend` z)@.

newtype ASemigroup a =
    ASemigroup {
      sappend :: a -> a -> a
    }
```

That's it.  Such a function is commonly referred to as a *composition*
function.  Given two arguments it *composes* them.  Here are a few
examples:

``` haskell
-- Compose two integers by addition.
sAdd :: ASemigroup Integer
sAdd = ASemigroup (+)

-- Compose two integers by multiplication.
sMult :: ASemigroup Integer
sMult = ASemigroup (*)

-- Compose two lists by concatenation.
sAppend :: ASemigroup [a]
sAppend = ASemigroup (++)

-- Compose two actions by IO composition.
sAndThen :: ASemigroup (IO a)
sAndThen = ASemigroup (>>)
```

In case you're wondering why I'm not talking about type classes right
now, it's because they're extra, and we'll talk about them later.  It's
better if we understand the essence of the matter first, before we
discuss the convenience features.

It's important to keep in mind that the `ASemigroup` type is tied to a
particular contract: the associativity law.  If we construct a value of
this type that doesn't satisfy it, we are breaking the contract.  Here
is an example of such a violation:

``` haskell
sBad :: ASemigroup Integer
sBad = ASemigroup (-)

-- Violation because: (3 - 2) - 1 ≠ 3 - (2 - 1)
```

Back to valid semigroups.  We can of course use any of these semigroups
directly, for which the `-XRecordWildCards` extension is useful,

``` haskell
{-# LANGUAGE RecordWildCards #-}

sAddExample :: Integer
sAddExample =
    let ASemigroup{..} = sAdd
    in 3 `sappend` 4 `sappend` 5 `sappend` 6

sAppendExample :: String
sAppendExample =
    let ASemigroup{..} = sAppend
    in foldr sappend [] ["A", "List", "Of", "Words"]
```

but more interestingly we can now abstract over semigroups (take
semigroups as arguments):

``` haskell
stimes' :: ASemigroup a -> Integer -> a -> a
stimes' sg@ASemigroup{..} n x =
    case compare n 1 of
      LT -> error "stimes: Non-positive count."
      EQ -> x
      GT -> x `sappend` stimes' sg (n - 1) x
```

This function, given a semigroup, a count `n` and a value `x` composes
`x` with itself the given number of times using the semigroup.
Examples:

``` haskell
stimes' add 4 10
  = 10 + (10 + (10 + 10))
  = 40

stimes' append 4 "Hello"
  = "Hello" ++ ("Hello" ++ ("Hello" ++ "Hello"))
  = "HelloHelloHelloHello"

stimes' andThen 4 (putStrLn "blah")
  = putStrLn "blah" >> (putStrLn "blah" >>
    (putStrLn "blah" >> putStrLn "blah"))
```

This seems like a lot of trouble to go through for what is essentially
just passing functions of type `(a -> a -> a)` around, so why is the
associativity law so useful that it warrants its own type?  Actually
associativity is a rather weak law and many functions satisfy it, but
the functions that do tend to be accumulating ("additive") in nature.
Another consequence of associativity is that we can regroup composition
as we wish, so given an expression like,

``` haskell
x1 `sappend` (x2 `sappend` (x3 `sappend` x4))
```

we can evaluate the following expression instead and get the same
result:

``` haskell
(x1 `sappend` x2) `sappend` (x3 `sappend` x4)
```

This ability to regroup actually gives rise to a few interesting
opportunities to optimise our code, which we will discover later.  For
now let me just give you a particular example.  Integer multiplication
is a semigroup:

``` haskell
((((((5 * 5) * 5) * 5) * 5) * 5) * 5) * 5
```

If you were to evaluate this the way it is presented, you would do seven
multiplications.  But since we're allowed to regroup as we wish, we can
compute the following instead,

``` haskell
  ((5 * 5) * (5 * 5)) * ((5 * 5) * (5 * 5))
= (25 * 25) * (25 * 25)
= 625 * 625
= 390625
```

which only cost three multiplications.

**Exercise 1**:  Construct a semigroup for the type `()`:

``` haskell
exercise1 :: ASemigroup ()
```

**Exercise 2**:  Which of the following are valid semigroups?

``` haskell
exercise2a :: ASemigroup (IO a)
exercise2a = ASemigroup (\c1 c2 -> c2 >> c1)

exercise2b :: ASemigroup Rational
exercise2b = ASemigroup (/)

exercise2c :: ASemigroup (Integer, Integer)
exercise2c = ASemigroup (\(x1, x2) (y1, y2) -> (x1 + y2, x2 + y1))

exercise2d :: ASemigroup Integer
exercise2d = ASemigroup max

exercise2e :: ASemigroup Integer
exercise2e = ASemigroup (\x y -> x * (-y))

exercise2f :: ASemigroup [a]
exercise2f = ASemigroup f
    where
    f xs     []     = xs
    f []     ys     = ys
    f (x:xs) (y:ys) = x : y : f xs ys
```

**Exercise 3 (hard)**:  Construct interesting semigroups for the
following types:

``` haskell
exercise3a :: ASemigroup (a -> a)

exercise3b :: ASemigroup (a -> Integer)

exercise3c :: ASemigroup (a -> IO a)
```


Semigroup morphisms
-------------------

Remember that a semigroup is an associative binary function.  We will
now make sense of functions that "go from one semigroup to another".
Let's take a look at the following function:

``` haskell
listLen :: [a] -> Integer
listLen = foldl' (\c _ -> 1 + c) 0
```

This function computes the length of the given list.  If you look at the
type, you can say that it goes from `[a]` to `Integer`, the underlying
types of the semigroups `sAppend` and `sAdd`.  But it also goes from
`(++)` to `(+)`, the underlying functions of the semigroups, in the
following sense:

  * For all `xs` and `ys`: `listLen (xs ++ ys) = listLen xs + listLen
    ys`.

We say that the `listLen` is a *semigroup morphism* or
*structure-preserving map* from the semigroup `sAppend` to the semigroup
`sAdd`.

More generally let `(!) :: A -> A -> A` be a semigroup over `A` and `(#)
:: B -> B -> B` be a semigroup over `B`, and let `f :: A -> B`.  Then
`f` is called a *semigroup morphism* from `(!)` to `(#)` if and only if
it satisfies the following law:

  * Structure preservation: for all `x :: A` and `y :: A`:  `f (x ! y) =
    f x # f y`.

Much like the associativity law the structure preservation law is weak
enough that we will find many semigroup morphisms, if we start actively
looking for them.

So what does this law actually mean, and why do we care?  Structure
preservation captures a very strong notion of locality and parallelism.
If a semigroup morphism cannot behave differently under composition, it
means that if it is passed a composite, it will use information of the
individual components only locally, even though it does not necessarily
know that it has received a composite.  Category theorists sometimes
call such a property *naturality*.  Software engineers might associate
this with *composability*.

The parallelism notion is equally interesting, because it is not just an
algebraic notion, but quite literally operational parallelism.  If `f`
is an expensive semigroup morphism, then instead of `f (x ! y)` you
might actually consider computing `f x # f y` instead, because now you
can compute `f x` and `f y` individually and in parallel.  This is
essentially how MapReduce works.

**TODO**: Add exercises.


Type classes
============

We have talked about the weakness of the associativity law, of which one
consequence is that there are loads and loads of semigroups.  I don't
just mean that in the obvious mathematical sense (there are infinitely
many of them), but that you actually find them all over your code, if
you train your senses to see them.  This also means that as you start
abstracting over semigroups it may get tedious to pass them around as
arguments all the time.  Luckily Haskell has a rather convenient feature
to pass stuff to functions implicitly: type classes.  A class for
semigroups looks like this:

``` haskell
-- | Instances of this class should satisfy the following law:
--
--   * Associativity: for all x, y, z,
--     @(x <> y) <> z = x <> (y <> z)@.

class Semigroup a where
    (<>) :: a -> a -> a
```

However, we can save ourselves the trouble of defining this class.
Since base 4.9 (GHC 8.0 and later) it comes as part of the base library
in the `Data.Semigroup` module.  If your base library is older, well,
you should update.  If for some reason you can't, you can install the
[semigroups library] for now.

[semigroups library]: https://hackage.haskell.org/package/semigroups

With this class we can dispense with the (admittedly rather ugly)
`ASemigroup` arguments and record wildcards when abstracting over
semigroups:

``` haskell
myStimes :: (Semigroup a) => Integer -> a -> a
myStimes n x =
    case compare n 1 of
      LT -> error "myStimes: Non-positive count"
      EQ -> x
      GT -> x <> myStimes (n - 1) x
```

The reason for the name `myStimes` is, as you may have guessed already,
that this function is actually predefined as `stimes`, except with an
optimisation that relies heavily on the associativity contract, and that
for many semigroups provides an exponential speedup (the
square-and-multiply algorithm).

There is a downside to the class-based approach though: semigroups are
now type-bound: you can only write one semigroup instance for `Integer`.
To overcome this limitation we use `newtype` as usual, and the following
types and instances are actually predefined:

``` haskell
newtype Sum a = Sum { getSum :: a }

instance (Num a) => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

newtype Product a = Product { getProduct :: a }

instance (Num a) => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)
```

When we use `myStimes` we can select the instance based on which wrapper
type we use:

``` haskell
myStimes 5 (Sum 3)     = Sum (3 + 3 + 3 + 3 + 3)
myStimes 5 (Product 3) = Product (3 * 3 * 3 * 3 * 3)
```

Even though there are almost always multiple semigroups for any given
type, for some types it's convenient to just declare one of them as
canonical and implement an instance for it.  For example for lists we
use the concatenation semigroup, and we would rarely choose another one,
so we implement:

``` haskell
instance Semigroup [a] where
    (<>) = (++)
```

From here on we will use type classes exclusively.  In other words, we
will discard the `ASemigroup` type in favour of the `Semigroup` class,
and we will identify types with semigroups, so for example instead of
referring to the "list of characters concatenation semigroup", we will
simply refer to the "`[Char]` semigroup".


Monoids
=======

Now that we understand semigroups, monoids are just an extra law away:
A monoid is a semigroup with a certain element called `mempty` that
satisfies the following equations for all `x`:

  * Left identity: `mempty <> x = x`,
  * Right identity: `x <> mempty = x`.

This law is called the identity law, and we will refer to `mempty` as
the *identity* of the monoid.  It is also sometimes called the *neutral
element*, the *empty element*, the *zero* or the *one*, which indicate
what the element denotes.

The type class for monoids is predefined (of course) and
straightforward... with a minor but rather unfortunate caveat: as of
October 2016 (base 4.9) there is a historical accident in the base
library.  It *should* be defined like this,

``` haskell
class (Semigroup a) => Monoid a where
    mempty :: a
```

but because the `Semigroup` class was actually added long after the
`Monoid` class, in the current base library it's defined like this:

``` haskell
class Monoid a where
    mappend :: a -> a -> a
    mempty  :: a
```

This may lead to some minor code duplication that in principle wouldn't
be necessary, but it's not a serious problem, and it will likely be
fixed in a future version.

Many semigroups are also monoids.  For example the list concatenation
semigroup is also a monoid,

``` haskell
instance Monoid [a] where
    mappend = (++)
    mempty = []
```

because concatenation with the empty list always yields the original
list.  The summation semigroup is also a monoid,

``` haskell
instance (Num a) => Monoid (Sum a) where
    mappend (Sum x) (Sum y) = Sum (x + y)
    mempty = Sum 0
```

because adding 0 always yields the original number.  Ok, let's look at a
slightly trickier example:

``` haskell
newtype AndThen = AndThen { runAndThen :: IO () }

instance Monoid AndThen where
    mappend (AndThen c1) (AndThen c2) =
        AndThen (c1 >> c2)

    mempty = _
```

This is supposedly the IO composition monoid.  We have seen that it's a
semigroup (`sAndThen`), so all we need for it to become a monoid is an
identity, an action that is neutral with respect to IO composition.
Which action could we compose with any other action to yield the
original action?  The action that does nothing.  If we compose any
action `c` with the action that does nothing, the result is just `c`
again.

``` haskell
mempty = AndThen (pure ())
```

Here is a family of semigroups that we have seen in exercise 2:

``` haskell
newtype Max a = Max { getMax :: a }

instance (Ord a) => Semigroup (Max a) where
    Max x <> Max y = Max (max x y)
```

Is `Max Word` a monoid?  Well, sure, the identity is `Max 0`, because
taking the maximum between the smallest possible number and any other
number always yields the other number.  Is `Max Int` a semigroup?  Yeah,
but the identity is now target-specific, because it's the smallest
possible `Int`.  But what about `Max Integer`?  Does that one have an
identity?  Nope.  There is no smallest integer.  A `Max` semigroup is a
monoid, if and only if it has a smallest element:

``` haskell
instance (Bounded a, Ord a) => Monoid (Max a) where
    mappend = (<>)
    mempty  = Max minBound
```

So sometimes a family of semigroups is partially a family of monoids,
but needs extra constraints.

**TODO**: Add exercises.


Monoid morphisms
----------------

**TODO**: Write slightly more than a TODO remark.


Monoidal design
---------------

There are a number of reasons why as a programmer one may find monoids
worth studying:

  * As a design pattern monoids capture the notion of *compositional
    design* (as opposed to monolithic design).  The key feature that
    follows from the monoid laws is that individual components must be
    self-contained in a strong sense, which enables local reasoning.

  * Monoid morphisms capture the notion of *extensibility*, but unlike
    traditional extension mechanisms monoid morphisms are strictly
    separate from the monoids they map.  In other words an extension
    will never change existing code.

Together these two features provide true separation of concerns.  Have
you ever called your code "done"?  Confidently?  Now you can.

**TODO**: Expand.
