---
title:     "Mastering foldr"
author:    "ertes"
date:      "2016-11-23 (draft)"
lang:      "en"
copyright: "Ertugrul Söylemez"
want-toc:  "yes"
...

I used to hate calculus; you know, that mathematical abomination that
calculates slopes of 15-dimensional curves or the exact area under
super-complicated graphs with a smile?  As an algebraic thinker it feels
wrong to me in so many ways, and yet I can't deny that it works.  It's
not *wrong*.  It's just *weird*.  Consider this: it takes all these
crazy edge cases of real analysis like infinitesimals, and reduces
everything to... a calculus!  A pragmatic and downright prosaic toolbox!
Something physicists and engineers carry around on their belts to get
their job done.  Today I kinda appreciate it for that.

Now you may be asking what this has to do with the topic of this
tutorial.  Folds and unfolds actually do something very similar to
calculus:  they take concepts like infinite recursion and
self-similarity and reduce them to a pair of functions with simple, well
understood semantics and properties.

Yet beginning Haskell programmers often struggle even with one of the
most fundamental list functions, `foldr`, and because of that they often
write longer and more complicated code than necessary.  This tutorial is
an attempt to improve the situation, not necessarily by going slower
than other tutorials, but by training readers to recognise patterns and
use equational reasoning to use them.  After all just like calculus
folds are *tools*, and you should master them.

This tutorial is written as a [literate Haskell file](foldr.lhs).


Folding Maybe
=============

Before we talk about `foldr` let's talk about its simpler cousin
`maybe`.  It's defined as:

``` haskell
maybe :: r -> (a -> r) -> Maybe a -> r
maybe n _ Nothing  = n
maybe _ j (Just x) = j x
```

This function basically replaces pattern-matching on `Maybe`.


Example: mLength
----------------

Suppose we want to write a function of type `Maybe [a] -> Int` that
returns the length of the argument list in the `Just` case and defaults
to 0 in the `Nothing` case.  We might do it by pattern-matching:

> mLengthExplicit :: Maybe [a] -> Int
> mLengthExplicit Nothing   = 0
> mLengthExplicit (Just xs) = length xs

Try the following examples in GHCi:

``` haskell
λ> mLengthExplicit (Just "abc")
3

λ> mLengthExplicit Nothing
0
```

Now we would like to write the same function (this time calling it
`mLength`) in terms of `maybe`.  The resulting function should be of the
following shape, which we will call its *template*:

``` haskell
mLength :: Maybe [a] -> Int
mLength = maybe _n _j
```

This means that we need to find `_n` and `_j` such that `maybe _n _j` is
the same function as `mLengthExplicit`.  As a useful feature note that
GHC treats variables that begin with an underscore (and are undefined)
specially as so-called *typed holes*.  When you try to compile this
code, it will actually tell you the types of `_n` and `_j`.

Okay, let's fill in the holes.  First let's consider what the result of
the function has to be in the `Nothing` case:

``` haskell
maybe _n _j Nothing = 0
```

If we substitute the definition of `maybe` for the `Nothing` case, we
arrive at:

``` haskell
_n = 0
```

We have found `_n`:

``` haskell
mLength = maybe 0 _j
```

In order to figure out what `_j` has to be we follow exactly the same
process.  We consider what the result of `maybe 0 _j (Just xs)` has to
be:

``` haskell
maybe 0 _j (Just xs) = length xs
```

Again substituting the definition of `maybe` we arrive at,

``` haskell
_j xs = length xs
```

or simply:

``` haskell
_j = length
```

And that completes our definition of `mLength`:

> mLength :: Maybe [a] -> Int
> mLength = maybe 0 length

If this felt a bit like school algebra, that's because it *is* school
algebra.  We have constructed equations and then solved them.  In fact
most school algebra questions were more involved, because in this case
all we had to do was to substitute definitions we had already known.


Example: flattenMaybe
---------------------

Let's do another example:  We would like to write a function that
flattens nested `Maybe` values:

> flattenMaybeExplicit :: Maybe (Maybe a) -> Maybe a
> flattenMaybeExplicit Nothing   = Nothing
> flattenMaybeExplicit (Just mx) = mx

Again we write our template and construct a system of equations to fill
in the two holes:

``` haskell
flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe = maybe _n _j

maybe _n _j Nothing   = Nothing
maybe _n _j (Just mx) = mx
```

Substituting the definition of `maybe` in our equations we get:

``` haskell
_n    = Nothing
_j mx = mx
```

So `_n` is just `Nothing`, and `_j` is the identity function, and we're
done:

> flattenMaybe :: Maybe (Maybe a) -> Maybe a
> flattenMaybe = maybe Nothing id

The `maybe` function is actually known as the `Maybe` fold.  It takes a
`Maybe` value and enough information to reduce it completely.  We will
see that `foldr` does basically the same thing for lists, but with a
twist: there is recursion involved.


Exercises
---------

Ready for a few exercises?  Keep in mind that you have two goals here:
the first goal is of course to solve these challenges, but the second
and more important goal is to use the kind of mechanical equational
reasoning that we have used above to do it.

Write `myMaybeToList` using the following template such that it
satisfies the following test cases:

``` haskell
-- Template:
myMaybeToList :: Maybe a -> [a]
myMaybeToList = maybe _n _j

-- Test cases:
myMaybeToList Nothing    = []
myMaybeToList (Just 'a') = "a"
```

Write `mApply` using the following template such that it satisfies the
following test cases:

``` haskell
-- Template:
mApply :: b -> Maybe (a -> b) -> a -> b
mApply defY mf x = maybe _n _j mf

-- Test cases:
mApply 0 Nothing 5     = 0
mApply 0 (Just (^2)) 5 = 25
```


foldr
=====

Let's review the definition of `foldr`.  For the majority of this
tutorial we will use a specialised version of `foldr`, namely the one
for lists:

``` haskell
foldr :: (a -> r -> r) -> r -> [a] -> r
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
```

This looks strikingly similar to `maybe` now, doesn't it?  The major
difference is that this one is recursive.  But don't worry: the method
we have used in the last section applies here almost unaltered, so let's
just dive straight into an example.


Example: reverse
----------------

> reverseExplicit :: [a] -> [a]
> reverseExplicit []     = []
> reverseExplicit (x:xs) = reverseExplicit xs ++ [x]

This is a function to reverse lists.  In order to write it in terms of
`foldr` first we write our template as before.  Let's call our function
`myReverse`, because `reverse` is predefined:

``` haskell
myReverse :: [a] -> [a]
myReverse = foldr _f _z
```

Next we construct our system of equations based on the semantics we
would like.  Let's do the two possible cases separately, starting with
the empty case:

``` haskell
foldr _f _z [] = []
```

We substitute the definition of `foldr` to arrive at:

``` haskell
_z = []
```

Okay, now that we know `_z` we can amend our template:

``` haskell
myReverse = foldr _f []
```

Now let's construct the equation for the non-empty case,

``` haskell
foldr _f [] (x:xs) = myReverse xs ++ [x]
```

and substitute the definition of `foldr` as usual:

``` haskell
_f x (foldr _f [] xs) = myReverse xs ++ [x]
```

Okay, this part is a bit tricky.  Let's make sure we understand this
equation properly.  First notice that `foldr _f []` is just `myReverse`:

``` haskell
_f x (myReverse xs) = myReverse xs ++ [x]
```

What this equation is really saying is that the function `_f` receives
as its second argument the reverse of `xs`, and its result should be the
reverse of `xs` with the singleton list `[x]` appended at its end.  In
other words the `myReverse xs` on the right hand side of the equation is
*not* a recursive application of `myReverse` to `xs`, but really just
the second argument of `_f`!  Here is perhaps a slightly clearer version
of the same equation:

``` haskell
_f x revOfXs = revOfXs ++ [x]
```

And that is actually already a valid definition for the function `_f`,
so we can use it:

> myReverse :: [a] -> [a]
> myReverse = foldr (\x revOfXs -> revOfXs ++ [x]) []


Example: printEach
------------------

Let's do another example.  The following function prints every list
element, each on a separate line:

> printEachExplicit :: (Show a) => [a] -> IO ()
> printEachExplicit [] = pure ()
> printEachExplicit (x:xs) =
>     print x >> printEachExplicit xs

Our template is,

``` haskell
printEach = foldr _f _z
```

with the following equations:

``` haskell
foldr _f _z []     = pure ()
foldr _f _z (x:xs) = print x >> printEach xs
```

As always we substitute:

``` haskell
_z = pure ()
_f x (foldr _f _z xs) = print x >> printEach xs
```

In the second equation we substitute `printEach` for `foldr _f _z`,

``` haskell
_f x (printEach xs) = print x >> printEach xs
```

and as before we notice that `printEach xs` on the right hand side is
not a recursive use of `printEach`, but just the second argument to
`_f`, so we rename `printEach xs` to `rest`:

``` haskell
_f x rest = print x >> rest
```

And with that we have found both `_f` and `_z`:

> printEach :: (Show a) => [a] -> IO ()
> printEach = foldr (\x rest -> print x >> rest) (pure ())


Example: append
---------------

Okay, let's do a slightly trickier example:

> appendExplicit :: [a] -> [a] -> [a]
> appendExplicit []     ys = ys
> appendExplicit (x:xs) ys = x : appendExplicit xs ys

This is the function to concatenate two lists.  In this case it is
important to identify on which of the two arguments the recursion
happens.  The first argument `xs` is the list we fold, while the second
argument `ys` is just a global constant we can refer to throughout the
fold.  Therefore our template looks like this:

``` haskell
append :: [a] -> [a] -> [a]
append xs ys = foldr _f _z xs
```

Here is our system of equations:

``` haskell
foldr _f _z [] = ys
foldr _f _z (x:xs) = x : append xs ys
```

Substitution again reveals the solutions:

``` haskell
_z = ys
_f x (foldr _f _z xs) = x : append xs ys
```

In the second equation notice that `foldr _f _z xs` is just `append xs
ys` per our definition above,

``` haskell
_f x (append xs ys) = x : append xs ys
```

and once again `append xs ys` is just the second argument:

``` haskell
_f x rest = x : rest
```

or much shorter:

``` haskell
_f = (:)
```

And that completes our definition:

> append :: [a] -> [a] -> [a]
> append xs ys = foldr (:) ys xs

This may still feel a bit alien; at least it did for me back when I was
struggling with list folds.  But the important things to keep in mind is
that you can substitute by equational reasoning freely (one of the main
strengths of Haskell!).  Practice a bit, and you will eventually get the
hang of it.


Exercises
---------

Implement `map` using the following template:

``` haskell
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr _f _z
```

Implement `concatMap` using the following template:

``` haskell
myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = foldr _f _z
```

Implement `filter` using the following template:

``` haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr _f _z
```


Stateful folds
==============

Here is an interesting challenge:  How can we implement `sum` or `take`
in terms of `foldr`?  These aren't as straightforward as the ones from
the previous section, because all our folds so far have been stateless.
But for example `take` has to maintain a decreasing counter during the
fold, and `sum` has to maintain a running sum.

At least for `sum`, if you have some Haskell experience, you might
conclude that it's just not a fold (also called a *right fold*, hence
the name `foldr`), but in fact what we call a *left fold*.  However, in
addition to learning what left folds are we will see whether they are
just special cases of folds.

With `take` the situation is even a bit more interesting.  It is a
stateful fold, but definitely not a left fold, because one property of
left folds is that they cannot return anything, not even lazily, before
the whole list has been traversed.  However, a bare fold the way we have
done it so far is stateless.

Do we need something more general than `foldr` to deal with state?


Append again
------------

In order to figure this out let's revisit one of the functions we have
written above: `append`.  What we have done is to just consider the
second argument as "extra information", something that is just a global
constant throughout the fold and used at the end of it:

``` haskell
append xs ys = foldr (:) ys xs
```

There is actually a different way to write this one.  Let's review its
recursive definition:

``` haskell
appendExplicit :: [a] -> [a] -> [a]
appendExplicit []     ys = ys
appendExplicit (x:xs) ys = x : appendExplicit xs ys
```

Pay attention to the structure of this function.  What if we consider
the fold to take a *single* argument `xs` and return a *function* of
`ys` instead of a list?  Due to currying this is just the same thing
written slightly differently:

``` haskell
appendExplicit :: [a] -> [a] -> [a]
appendExplicit []     = id
appendExplicit (x:xs) = \ys -> x : appendExplicit xs ys
```

But now our template looks quite different:

``` haskell
append2 :: [a] -> [a] -> [a]
append2 = foldr _f _z

-- Old template for comparison:
append xs ys = foldr _f _z xs
```

Consequently our system of equations also looks different:

``` haskell
foldr _f _z []     = id
foldr _f _z (x:xs) = \ys -> x : append2 xs ys
```

Nevertheless the end result should still be the same, because we are
really just using a different derivation method.  So let's just continue
and see what we get.  Substitution gives us:

``` haskell
_z = id
_f x (foldr _f _z xs) = \ys -> x : append2 xs ys
```

Once again we tie the recursive knot for the non-empty case:

``` haskell
_f x (append2 xs) = \ys -> x : append2 xs ys
```

Finally we rewrite this a bit, moving the lambda to the left hand side
and renaming `append2 xs` to `more`:

``` haskell
_f x more ys = x : more ys
```

This leads to a seemingly different implementation from what we had
before:

> append2 :: [a] -> [a] -> [a]
> append2 = foldr (\x more ys -> x : more ys) id

Here is the old version for comparison:

``` haskell
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
```

Are these really the same function?  Well, they have to be!  And indeed,
a quick experiment at the GHCi prompt confirms it, at least empirically:

``` haskell
λ> append2 "abc" "def"
"abcdef"
```

Okay, what happened here?  You have just witnessed a trivial example of
stateful folds.  Instead of returning the concatenated list, this fold
returns a *function* that takes the latter part of the list (the `ys`)
and while it consumes the argument list (`xs`), it just keeps passing
`ys` to the next recursion level unchanged (`more ys`).  At the very end
of the fold, when `xs` is fully consumed and it has to decided what the
remainder of the list should be, the fold decides that it should be the
current state (`id`, or equivalently: `\ys -> ys`), which of course is
just the initial `ys`, because the state was never changed.


Example: take
-------------

Alright, let's tackle the `take`.  As always let's look at a recursive
definition first:

> takeExplicit :: Int -> [a] -> [a]
> takeExplicit n (x:xs) =
>     if n > 0
>       then x : takeExplicit (n - 1) xs
>       else []
> takeExplicit _ [] = []

If we try to solve this directly, we will get the following template and
equations (after all substitutions):

``` haskell
myTake n = foldr _f _z

_z = []

_f x (myTake n xs) =
    if n > 0
      then x : myTake (n - 1) xs
      else []
```

And now we have a problem.  The function `_f` receives `myTake n xs` as
its argument, but to proceed it would need `myTake (n - 1) xs`.  There
is no way to solve this equation (other than to actually use `myTake`
recursively, which would defeat the point of `foldr`).  The problem is
that we cannot treat `n` as a global constant.  It has to be *state*
that we can *change* during the fold, just like `takeExplicit` does.

The idea now is to construct the result indirectly:  Construct a
function that takes as its argument the number of elements to take.
This causes `_f` to receive a *function* of the remaining number of
elements, so that it can change it in the recursive application.  This
turns `n` from a global constant to an *initial state*.  We have done
the same with `append2` earlier, except we never actually changed the
state.

However, `myTake` is not quite in a suitable form to allow this
transformation, because here the initial state argument comes *before*
the list.  We need to construct our fold for a flipped variant to make
currying work for us just like it did with `append2`:

``` haskell
flippedTakeExplicit :: [a] -> Int -> [a]
flippedTakeExplicit (x:xs) =
    \n ->
        if n > 0
          then x : flippedTakeExplicit xs (n - 1)
          else []
flippedTakeExplicit [] = const []
```

Here is the template and the equations:

``` haskell
flippedTake :: [a] -> Int -> [a]
flippedTake = foldr _f _z

foldr _f _z [] = const []

foldr _f _z (x:xs) =
    \n ->
        if n > 0
          then x : flippedTake xs (n - 1)
          else []
```

After initial substitutions we get our solution for `_z` and a promising
equation for `_f`:

``` haskell
_z = const []

_f x (foldr _f _z xs) =
    \n ->
        if n > 0
          then x : flippedTake xs (n - 1)
          else []
```

This time `foldr _f _z` is equal to `flippedTake`:

``` haskell
_f x (flippedTake xs) =
    \n ->
        if n > 0
          then x : flippedTake xs (n - 1)
          else []
```

Great!  This function receives `flippedTake xs` as its second argument,
and that is exactly what it needs in order to continue.  As usual we
rewrite it a bit to be more readable:

``` haskell
_f x more n =
    if n > 0
      then x : more (n - 1)
      else []
```

And finally we use it to complete our fold:

> flippedTake :: [a] -> Int -> [a]
> flippedTake = foldr f (const [])
>     where
>     f x more n =
>         if n > 0
>           then x : more (n - 1)
>           else []

Now we could define `myTake` as `flip flippedTake`, but it's probably
better to just discard `flippedTake` and write `myTake` directly:

> myTake :: Int -> [a] -> [a]
> myTake n xs = foldr f (const []) xs n
>     where
>     f x more n =
>         if n > 0
>           then x : more (n - 1)
>           else []

So what have we learned so far?  For stateful folds we have to keep in
mind that the result of the fold has to be a function:

``` haskell
flippedTake :: [a] -> (Int -> [a])
```

That means that the `r` in the type signature of `foldr` becomes a
function type:

``` haskell
-- Stateful folds
foldr
    :: (a -> (s -> r) -> s -> r)
    -> (s -> r)
    -> [a]
    -> s -> r
```

It also means that the first argument is a function that receives
*three* arguments: the current list element, the remainder of the fold
(parameterised on next/updated state) as well as the current state.  By
the way, nothing stops us from introducing additional state arguments,
should we need them, which is a bit nicer than tuples:

``` haskell
-- Stateful folds with two state variables
foldr
    :: (a -> (s1 -> s2 -> r) -> s1 -> s2 -> r)
    -> (s1 -> s2 -> r)
    -> [a]
    -> s1 -> s2 -> r
```

This also answers our question from earlier:  `foldr` can handle
stateful folds on its own, so we don't need something more powerful.


Example: sum
------------

Alright, next example: `sum` or rather `mySum`.  We don't want the naive
version, but the efficient one, which looks like this:

> mySumExplicit :: (Num a) => [a] -> a
> mySumExplicit = go 0
>     where
>     go s' [] = s'
>     go s' (x:xs) =
>         let s = s' + x
>         in s `seq` go s xs

The whole recursion happens in `go`, so that is the function we would
turn into a fold.  But just like with `take` it's currently not in the
right shape, because state arguments have to come *after* the list:

> mySumExplicit2 :: (Num a) => [a] -> a
> mySumExplicit2 xs = go xs 0
>     where
>     go [] s' = s'
>     go (x:xs) s' =
>         let s = s' + x
>         in s `seq` go xs s

Let's turn everything into lambda form one last time (we will do this in
our head in the future):

> mySumExplicit3 :: (Num a) => [a] -> a
> mySumExplicit3 xs = go xs 0
>     where
>     go [] = \s' -> s'
>     go (x:xs) =
>         \s' ->
>             let s = s' + x
>             in s `seq` go xs s

Now we can write our template and equations:

``` haskell
mySum :: (Num a) => [a] -> a
mySum xs = foldr _f _z xs 0

foldr _f _z [] = id
foldr _f _z (x:xs) =
    \s' ->
        let s = s' + x
        in s `seq` foldr _f _z xs s
```

Notice a slight difference:  In all our previous examples the fold had
its own name like `myReverse = foldr _f _z`, so we used that name in the
recursive case.  But here our fold doesn't actually have a name, so we
just use `foldr _f _z` directly on the right hand side.  From here we
proceed as usual:

``` haskell
_z = id

_f x (foldr _f _z xs) =
    \s' ->
        let s = s' + x
        in s `seq` foldr _f _z xs s
```

Once again notice that the `foldr _f _z xs` on the right hand side is
just the second argument to `_f`, and we move the lambda to the other
side, too:

``` haskell
_f x more s' =
    let s = s' + x
    in s `seq` more s
```

This can be written much more succinctly using the `($!)` operator:

``` haskell
_f x more s = more $! s + x
```

And thus we're done:

> mySum :: (Num a) => [a] -> a
> mySum xs =
>     let f x more s = more $! s + x
>     in foldr f id xs 0


Example: foldl'
---------------

So what are these left folds we have been talking about?  Let me just
show you one way to define them:

> foldl'Explicit :: (r -> a -> r) -> r -> [a] -> r
> foldl'Explicit f = go
>     where
>     go s' [] = s'
>     go s' (x:xs) =
>         let s = f s' x
>         in s `seq` go s xs

Doesn't that look familiar?  Yeah, almost like we have written the same
thing just a moment ago.  Indeed, this function looks a lot like
`mySumExplicit`, except that it abstracts over the way the state is
combined with the next list element, and over the initial state.  If we
had defined this function earlier, we could have written `mySum` simply
as:

> mySum2 :: (Num a) => [a] -> a
> mySum2 = foldl'Explicit (+) 0

But at the same time we were able to define `mySum` in terms of `foldr`
just as well, except that it was slightly longer, and the similarity
suggests that we should be able to write `foldl'` as an actual fold,
too.  So what if we define left folds in terms of `foldl'` and `foldl'`
in terms of `foldr`?  Let's see if we can.

Before we start a note about the strange name:  Why do we call it
`foldl'` and not just `foldl`?  Good question.  Whimsy, I guess.  Well,
there is this conspiracy theory that if you ever use the name `foldl` in
your code, an evil demon created by the secret Haskell committee will
appear, rip apart your computer and eat all your RAM.  Some people have
reported sightings of the creature, but before they could ever take a
picture, their system would crash.

Alright, enough horror stories.  Once again we need to flip the inner
function.

> foldl'Explicit2 :: (r -> a -> r) -> r -> [a] -> r
> foldl'Explicit2 f s0 xs = go xs s0
>     where
>     go [] s' = s'
>     go (x:xs) s' =
>         let s = f s' x
>         in s `seq` go xs s

From that we can write our template:

``` haskell
myFoldl' :: (r -> a -> r) -> r -> [a] -> r
myFoldl' f s0 xs = foldr _f _z xs s0
```

This time we won't even bother turning everything into lambdas first.
We just turn the template as is into equations, so we get an extra
argument `s'` everywhere:

``` haskell
foldr _f _z [] s' = s'
foldr _f _z (x:xs) s' =
    let s = f s' x
    in s `seq` foldr _f _z xs s
```

And substitute:

``` haskell
_z s' = s'
_z = id

_f x (foldr _f _z xs) s' =
    let s = f s' x
    in s `seq` foldr _f _z xs s
```

And rename:

``` haskell
_f x more s' =
    let s = f s' x
    in s `seq` more s
```

And use `($!)`:

``` haskell
_f x more s = more $! f s x
```

And complete:

> myFoldl' :: (r -> a -> r) -> r -> [a] -> r
> myFoldl' f s0 xs =
>     let g x more s = more $! f s x
>     in foldr g id xs s0

So indeed left folds are just special right folds, but they are quite
handy for some stateful folds, most notably the strict ones.  Examples
include calculating sums (initial state 0, add every element), products
(initial state 1, multiply by every element),

> myProduct :: (Num a) => [a] -> a
> myProduct = myFoldl' (*) 1

and the function that computes the length of the argument list (start at
0, increment state for every list element):

> myLength :: [a] -> Int
> myLength = myFoldl' (\s _ -> s + 1) 0

However, keep in mind that left folds are always strict in the full
list, not necessarily the *elements*, but they will always reach the
empty list case before they can answer.  That's why there aren't too
many use cases for left folds.


Example: foldl
--------------

Alright, alright, I was lying about the evil demon.  There is an actual
`foldl` function that is pretty much the same as `foldl'`, except that
it doesn't force evaluation of the state at every step, so it builds up
an expression in memory as it traverses the list:

> myFoldl :: (s -> a -> s) -> s -> [a] -> s
> myFoldl f s0 xs =
>     foldr (\x more s -> more (f s x)) id xs s0

Since an unevaluated expression usually isn't what we would want, this
function is rarely useful.  The reason is that `foldl` still cannot
produce, not even lazily, before the whole list is traversed.  In fact I
couldn't think of any use cases, until I was made aware of `reverse`,
which is indeed a really good left fold:

> myReverse2 :: [a] -> [a]
> myReverse2 = myFoldl (flip (:)) []

In fact this definition of `reverse` is far better than our last one
that used `(++)`.  In general if you're building something *from the
back*, and there would be no benefit in forced evaluation, then `foldl`
may be the function to use.


Exercises
---------

Implement `drop` using the following template:

``` haskell
myDrop :: Int -> [a] -> [a]
myDrop n xs = foldr _f _z xs n
```

Implement a function `repInc` ("replicate increasingly") using the
template below such that it satisfies the test cases below.  It should
replicate the first element once, the second element twice, the third
element three times, etc.

``` haskell
-- Template:
repInc :: [a] -> [a]
repInc xs = foldr _f _z xs 1

-- Test cases:
repInc "a"     = "a"
repInc "abc"   = "abbccc"
repInc "abcde" = "abbcccddddeeeee"

take 10 (repInc [1..]) = [1,2,2,3,3,3,4,4,4,4]
```

Implement `(!!?)` using the template below such that it satisfies the
test cases below.  It should return the element at the given position,
if it exists.

``` haskell
-- Template:
(!!?) :: [a] -> Int -> Maybe a
(!!?) = foldr _f _z

-- Test cases:
"abcde" !!? 0 = Just 'a'
"abcde" !!? 2 = Just 'c'
"abcde" !!? 4 = Just 'e'
"abcde" !!? 6 = Nothing
"abcde" !!? 8 = Nothing
"abcde" !!? (-1) = Nothing
```

Implement `dropEveryOther` using the template below.  It should drop
every other element of the argument list.

``` haskell
-- Template:
dropEveryOther :: [a] -> [a]
dropEveryOther xs = foldr _f _z xs True

-- Test cases:
dropEveryOther "abcdef"  = "ace"
dropEveryOther "abcdefg" = "aceg"

take 5 (dropEveryOther [0..]) = [0,2,4,6,8]
```


Reverse state
=============

Now that we have mastered the algebraic part of the story it's time to
get a more intuitive understanding.  The goal is to be able to write
folds *directly*, not by deriving them from an explicit recursive
definition.  This intuition will not only make us better at *writing*
folds, but will also help us at *reading* them.

The state we have seen in the last section has been "forward" (or
perhaps "inward") in the sense that we started at the head of the list
with an initial state and then manipulated it as we traversed the list.
Then `_z` (which is a function in the stateful case) would receive the
final state.  As the name suggests *reverse state* is "backward" (or
"outward") in the sense that the initial state is given by `_z` and is
manipulated in the outward direction.

Here is the enlightening part: all folds involve reverse state.  In fact
folds are *completely about* reverse state.  Let's look at a few simple
examples.


The identity fold
-----------------

Is there a fold that just returns the original list unchanged?  In other
words, are there `_f` and `_z` such that `foldr _f _z = id`?  Let's see:

``` haskell
foldr _f _z [] = id []
foldr _f _z [] = []
_z = []

foldr _f _z (x:xs) = id (x:xs)
foldr _f _z (x:xs) = x:xs
_f x (foldr _f _z xs) = x:xs
_f x (id xs) = x:xs
```

We're stuck with the second equation, but in order to continue we just
need to realise that `xs = id xs`:

``` haskell
_f x (id xs) = x : id xs
_f x more = x : more
_f = (:)
```

Indeed, there is an identity fold:

> idFold :: [a] -> [a]
> idFold = foldr (:) []

Now let's step back for a moment and review what the recursive
definition would have looked like:

> idFoldExplicit :: [a] -> [a]
> idFoldExplicit [] = []
> idFoldExplicit (x:xs) = x : idFoldExplicit xs

And let me rewrite the non-empty clause slightly:

``` haskell
idFoldExplicit :: [a] -> [a]
idFoldExplicit [] = []
idFoldExplicit (x:xs) =
    let s = idFoldExplicit xs
    in x : s
```

Even though it's not quite the same thing as `id`, because it does
actually traverse the list, it's semantically indistinguishable from
`id`.  But more interestingly there is a pattern:

  * I got `x : xs`.
  * What's my result for `xs`?
  * Let's call that one `s`.
  * Then the result for `x : xs` is `x : s`.

This is actually a stateful pattern, except that the current state is
not recevied from outside and then sent to deeper recursion levels
potentially modified, but rather that the current state is received from
inside, from deeper recursion levels, modified (by prepending the `x`)
and then sent outward ("sending outward" really just means "returning").
Also instead of receiving the initial state from outside, here the
initial state is given by the base case for the empty list, i.e. the
innermost layer of recursion.  It's *reverse state*, state with the
propagation direction flipped.

But something is interesting about reverse state: the base case (the
initial state) does not have to be reachable.  Try `idFold` for an
infinite list, and it will still work.  The reason is that the `(:)`
constructor isn't actually strict in its second argument and therefore
not strict in the initial state.  If you pattern-match on `idFold
[1..]`, then the answer is `1 : idFold [2..]`.  In order to return the
head, the previous state wasn't even needed.


A semi-constant fold
--------------------

Let's try something:

> finite :: [a] -> ()
> finite = foldr (\_ s -> s) ()

What is this?  Give it a list `x : xs`, and it will ask itself: "What is
my result for `xs`?"  It will call that result `s`.  Then it will just
return `s` unchanged.  For finite lists the fold will eventually reach
the end of the list, at which point it will know what the initial `s`
is: `()`.  Since it was never changed (remember that `_f` just returned
the `s` that it received), the final result of the fold is also `()`.

However, for infinite lists the fold will never actually reach a point
when it can know what the initial state is.  What is `finite [0..]`?

``` haskell
  finite [0..]
= finite (0 : [1..])
= (\_ s -> s) 0 (finite [1..])
= finite [1..]
```

Evidently this will keep going forever.  Therefore `finite` isn't
actually a constant function.  It will eventually return `()` for every
finite list, but for infinite lists the result is undefined, or as we
like to call it in Haskell, the result is `⊥` ("bottom").

A different (but equivalent) way to explain why this function never
returns for infinite lists is that its `_f` is clearly strict in its
second argument: `_f x s = s`, therefore `_f x ⊥ = ⊥`.  Therefore it
will depend on the recursive result, which will in turn depend on the
recursive result, etc.


Example: null
-------------

Regarding the amount of traversal done by a fold the `null` function is
in a sense the exact opposite of the `finite` function, because except
for one edge case it will never traverse the full list.

> myNull :: [a] -> Bool
> myNull = foldr (\_ _ -> False) True

If you look at the way `_f` is defined, it's really obvious:  When this
function receives `x : xs`, it doesn't even ask the question what its
result for `xs` is.  It just returns `False` and is done.  The only way
this function can return `True` is when it receives `[]` right away,
because every non-empty list would cause it to return `False`.

Here `_f` is obviously non-strict.  But not only that, the recursive
result isn't used at all, so even if you would evaluate this fold's
result to normal form, it still wouldn't traverse the list.  There is
only one case when `null` looks at the entire list, and that is the
empty list.


Heads, tails and a digression
-----------------------------

Usage of the predefined functions `head` and `tail` is a clear sign of
*boolean blindness*.  Let me show you an example to explain what this
means:

> printHeadBlind :: (Show a) => [a] -> IO ()
> printHeadBlind xs =
>     if null xs
>       then putStrLn "Empty."
>       else {- blind: -} print (head xs)

The *boolean-blind* region of this code is the `else` branch.  We (the
programmer) have established as a fact that the argument we give to
`head` can never be the empty list, in this case by branching on `null
xs`, but within the boolean-blind region our program forgot that fact
and used the uninformed `head`, which in turn has to check whether the
list is empty or not.  The `printHeadBlind` function *is* safe, but that
safety was not established *by construction*, but rather by us, the
programmer, knowing more about the circumstances than the code itself.
This is a recipe for disaster, once we have to refactor our code.

The reason this code is boolean-blind is that the `null` function
reduces its knowledge about the shape of the list to literally a single
bit of information, but in the `else` branch we would have needed more
than that.  The way to eliminate boolean-blindness thus is to *preserve*
all the information we need.  Enter `safeHead`:

> safeHead :: [a] -> Maybe a
> safeHead = foldr (\x _ -> Just x) Nothing

This function returns both the information whether the list is empty or
not *and* the head element.  Then instead of `if`-branching on a boolean
we would `maybe`-branch on the result of `safeHead`:

> printHead :: (Show a) => [a] -> IO ()
> printHead =
>     maybe (putStrLn "Empty.") print .
>     safeHead

This function is safe by construction.  It has no boolean-blind regions.

But notice what the code does:  It translates the argument list to a
`Maybe`, and then folds it using `Maybe`.  Why don't we just fold the
list directly?

> printHead2 :: (Show a) => [a] -> IO ()
> printHead2 =
>     foldr (\x _ -> print x)
>           (putStrLn "Empty.")

And that raises the question:  Do we actually ever need the `safeHead`
function?  We can always just use `foldr` and save ourselves the trouble
of converting to `Maybe` first, which, if you think about it, is just a
list type with a maximum length of 1.

What about `tail`?  Well, we will talk about that one in the exercises
below. =)


Example: bisect
---------------

For some algorithms like merge-sort it's useful to bisect lists.  You
might be thinking of breaking them apart in the middle, but that would
require knowing the length.  A better idea is to split lists by
alternating elements (think of unzipping).  Here is the type signature:

``` haskell
bisect :: [a] -> ([a], [a])
```

And here are a few test cases:

``` haskell
bisect []        = ([],    []   )
bisect [0]       = ([0],   []   )
bisect [0,1]     = ([0],   [1]  )
bisect [0,1,2]   = ([0,2], [1]  )
bisect [0,1,2,3] = ([0,2], [1,3])

bisect [0,1,2,3,4,5,6,7,8,9] =
    ([0,2,4,6,8], [1,3,5,7,9])
```

There are a few ways to do this, but of course we are interested in a
way that can be expressed as a fold, which means that we can only ever
look at a single list element at any step.  If you're not used to this
restriction, this might actually be quite challenging, so I invite you
to stop here and just try to do it on your own first.

Given an element `x` and `more = bisect xs`, what would `bisect (x :
xs)` be?  Template:

``` haskell
bisect :: [a] -> ([a], [a])
bisect = foldr (\x more -> _) ([], [])
```

Well, `more` is a tuple, and we would prepend `x` to one of its
components, but which one?

``` haskell
bisect =
    foldr (\x more ->
               let (ys1, ys2) = more
               in _)
          ([], [])
```

There is actually a really simple way to do it.  But first let's look at
a more complicated way.  In the template `more` is a tuple:

If we just prepend to the left component `ys1`, then we will always
prepend to the left component recursively and ultimately leave the right
component empty.  Same for `ys2`.  What we need is some kind of state
that allows us to go back and forth between the two.  We could just add
a piece of boolean state to do that:

> bisectS :: [a] -> ([a], [a])
> bisectS xs =
>     foldr (\x more l ->
>                let (ys1, ys2) = more (not l)
>                in if l
>                     then (x:ys1, ys2)
>                     else (ys1, x:ys2))
>           (const ([], []))
>           xs
>           True

However, as noted there is a much more elegant way using reverse state:
Just swap the tuples:

> bisect :: [a] -> ([a], [a])
> bisect =
>     foldr (\x more ->
>                let (ys1, ys2) = more
>                in (x:ys2, ys1))
>           ([], [])

Nice, isn't it?  Let's clean this up a bit:

> bisect' :: [a] -> ([a], [a])
> bisect' =
>     foldr (\x (ys1, ys2) -> (x:ys2, ys1))
>           ([], [])

Even nicer!

But I called this one `bisect'`, and you might be wondering why.  Well,
it's not the same function as `bisect`!  There is a very subtle
difference that could be overlooked easily:  this one is strict in the
tuple that we used to call `more`, which means that it insists on
reaching the base case before the component lists are defined.

Patterns bound by a lambda are strict by default in Haskell, but
patterns bound by `let` are not.  That's why `bisect` works on infinite
lists, but `bisect'` doesn't.  Even for finite lists `bisect'` now shows
terrible memory behaviour, because as it waits for the base case to be
reached it builds up an expression in memory.

The way to fix it is to use a lazy (a.k.a. *irrefutable*) pattern:

> bisect2 :: [a] -> ([a], [a])
> bisect2 =
>     foldr (\x ~(ys1, ys2) -> (x:ys2, ys1))
>           ([], [])

This one is actually equivalent to `bisect`.


Exercises
---------

TODO


Exercise solutions
==================

Assuming that you are reading this visually and use a somewhat modern
browser the solutions are initially invisible, and you need to hover
with your pointing device over them to see them.  That way you can view
individual solutions without spoiling the rest.

<div class="hover-show">
**myMaybeToList:**

> myMaybeToList :: Maybe a -> [a]
> myMaybeToList = maybe [] (: [])

Derivation:

``` haskell
myMaybeToList = maybe _n _j

maybe _n _j Nothing = []
_n = []

maybe _n _j (Just x) = x : []
_j x = x : []
_j = (: [])
```
</div>

<div class="hover-show">
**mApply:**

> mApply :: b -> Maybe (a -> b) -> a -> b
> mApply defY mf x = maybe defY ($ x) mf

Derivation:

``` haskell
mApply _n _j Nothing = defY
_n = defY

mApply _n _j (Just f) = f x
_j f = f x
_j f = f $ x
_j = ($ x)
```
</div>

<div class="hover-show">
**myMap:**

> myMap :: (a -> b) -> [a] -> [b]
> myMap f = foldr (\x ys -> f x : ys) []

Derivation:

``` haskell
foldr _f _z [] = []
_z = []

foldr _f _z (x:xs) = f x : myMap f xs
_f x (foldr _f _z xs) = f x : myMap f xs
_f x (myMap f xs) = f x : myMap f xs
_f x ys = f x : ys
```
</div>

<div class="hover-show">
**myConcatMap:**

> myConcatMap :: (a -> [b]) -> [a] -> [b]
> myConcatMap f = foldr (\x ys -> f x ++ ys) []

Derivation:

``` haskell
foldr _f _z [] = []
_z = []

foldr _f _z (x:xs) = f x ++ myConcatMap f xs
_f x (foldr _f _z xs) = f x ++ myConcatMap f xs
_f x (myConcatMap f xs) = f x ++ myConcatMap f xs
_f x ys = f x ++ ys
```
</div>

<div class="hover-show">
**myFilter:**

> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter p =
>     foldr (\x ys -> if p x then x : ys else ys) []

Derivation:

``` haskell
foldr _f _z [] = []
_z = []

foldr _f _z (x:xs) = if p x then x : myFilter p xs else myFilter p xs
_f x (foldr _f _z xs) = if p x then x : myFilter p xs else myFilter p xs
_f x (myFilter p xs) = if p x then x : myFilter p xs else myFilter p xs
_f x ys = if p x then x : ys else ys
```
</div>

<div class="hover-show">
**myDrop:**

> myDrop :: Int -> [a] -> [a]
> myDrop n xs = foldr f (const []) xs n
>     where
>     f x more n =
>         if n <= 0
>           then x : more 0
>           else more (n - 1)

Derivation:

``` haskell
foldr _f _z [] n = []
_z n = []
_z = const []

foldr _f _z (x:xs) n =
    if n <= 0
      then x : xs
      else foldr _f _z xs (n - 1)

_f x (foldr _f _z xs) n =
    if n <= 0
      then x : xs  -- xs is not in scope, but ...
      else foldr _f _z xs (n - 1)

_f x (foldr _f _z xs) n =
    if n <= 0
      then x : myDrop 0 xs  -- xs = myDrop 0 xs
      else foldr _f _z xs (n - 1)

_f x (foldr _f _z xs) n =
    if n <= 0
      then x : foldr _f _z xs 0
      else foldr _f _z xs (n - 1)

_f x more n =
    if n <= 0
      then x : more 0
      else more (n - 1)
```
</div>

<div class="hover-show">
**repInc:**

> repInc :: [a] -> [a]
> repInc xs = foldr f (const []) xs 1
>     where
>     f x more n = replicate n x ++ more (n + 1)

Derivation:

``` haskell
foldr _f _z [] n = []
_z n = []
_z = const []

foldr _f _z (x:xs) n = replicate n x ++ foldr _f _z xs (n + 1)
_f x (foldr _f _z xs) n = replicate n x ++ foldr _f _z xs (n + 1)
_f x more n = replicate n x ++ more (n + 1)
```
</div>

<div class="hover-show">
**(!!?):**

> (!!?) :: [a] -> Int -> Maybe a
> (!!?) = foldr f (const Nothing)
>     where
>     f x _    0 = Just x
>     f _ more n = more (n - 1)

Derivation method 1:

``` haskell
foldr _f _z [] n = Nothing
_z n = Nothing
_z = const Nothing

foldr _f _z (x:xs) n =
    if n == 0 then Just x else xs !!? (n - 1)

_f x (foldr _f _z xs) n =
    if n == 0 then Just x else xs !!? (n - 1)

_f x (foldr _f _z xs) n =
    if n == 0 then Just x else foldr _f _z xs (n - 1)

_f x more n =
    if n == 0 then Just x else more (n - 1)
```

Derivation method 2:

``` haskell
foldr _f _z [] n = Nothing
_z n = Nothing
_z = const Nothing

foldr _f _z (x:xs) 0 = Just x
_f x (foldr _f _z xs) 0 = Just x
_f x _ 0 = Just x

foldr _f _z (x:xs) n | n /= 0 = xs !!? (n - 1)
_f x (foldr _f _z xs) n | n /= 0 = xs !!? (n - 1)
_f x (foldr _f _z xs) n | n /= 0 = foldr _f _z xs (n - 1)
_f x more n | n /= 0 = more (n - 1)
```
</div>

<div class="hover-show">
**dropEveryOther:**

> dropEveryOther :: [a] -> [a]
> dropEveryOther xs = foldr f (const []) xs True
>     where
>     f _ more False = more True
>     f x more True  = x : more False

Derivation method 1:

``` haskell
foldr _f _z [] b = []
_z b = []
_z = const []

foldr _f _z (x:xs) b =
    if b
      then x : foldr _f _z xs False
      else foldr _f _z xs True
_f x (foldr _f _z xs) b =
    if b
      then x : foldr _f _z xs False
      else foldr _f _z xs True
_f x more b =
    if b
      then x : more False
      else more True
```

Derivation method 2:

``` haskell
foldr _f _z [] b = []
_z b = []
_z = const []

foldr _f _z (x:xs) False = foldr _f _z xs True
_f x (foldr _f _z xs) False = foldr _f _z xs True
_f x more False = more True

foldr _f _z (x:xs) True = x : foldr _f _z xs False
_f x (foldr _f _z xs) True = x : foldr _f _z xs False
_f x more True = x : more False
```
</div>
