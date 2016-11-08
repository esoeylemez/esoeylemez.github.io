---
title:     "Monoids for configuration"
author:    "ertes"
date:      "2016-11-08"
lang:      "en"
copyright: "Ertugrul Söylemez"
want-toc:  "yes"
...

[generic-deriving package]: https://hackage.haskell.org/package/generic-deriving

Configuring an application usually involves gathering information from
multiple sources, for example:

  * command line arguments,
  * environment variables,
  * configuration files,
  * etc.

This article introduces my method for doing this algebraically using
monoids.


Introduction
============

Imagine a hypothetical compiler named `myc`, which compiles the
hypothetical language MyLang, version 2 by default.  If you ask for a
command line synopsis, it will report:

    % myc --help
    myc [-dv] [-l v1|v2] [-o outputfile] [inputfile...]
      -d  Enable debugging information
      -l  Language version
      -o  Set output file
      -v  Be more verbose

In addition to the command line flags it also reads the `MYCFLAGS`
environment variable for default options, and each input file can
provide further options by using a special pragma at the top of the
file.  This means that there are three sources for configuration data,
each with different priorities, and one of them is special, as we will
see later.  Applications that are configurable by the user in that
fashion generally have at least two phases:

  * the *build* phase, during which configuration data is gathered,

  * the *run* phase, during which the configuration data is used.

In the build phase configuration data is gathered from all relevant
sources and then combined monoidally, because monoids are the algebraic
notion for *accumulation*.  Examples:

  * Verbosity levels are additive (more `-v` options = more verbose),

  * Lists of input files are concatenative (or alternatively union-like,
    if you use *sets* instead of lists),

  * The language version follows the `Last` monoid (the most recent
    option wins).

The data gathered during this phase can be captured as a *product
monoid*, that is simply a record type where each field is a monoid.  We
will use GHC generics together with the [generic-deriving package],
which writes instances for product monoids for us (an otherwise
mechanical and rather tedious process):

``` haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Set (Set)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid

data Language = V1 | V2

data ConfigBuild =
    ConfigBuild {
      _debug     :: Any,
      _inFiles   :: Set FilePath,
      _language  :: Last Language,
      _outFile   :: Last FilePath,
      _verbosity :: Sum Integer
    }
    deriving (Generic)

instance Monoid ConfigBuild where
    mappend = mappenddefault
    mempty = memptydefault
```

When the build phase is over we would switch into the run phase, at
which point we have to use a slightly different type, because some of
the fields are no longer monoids (although in most cases they could
still be semigroups).  For example the language version has to be
definite, if necessary by using a default:

``` haskell
{-# LANGUAGE DuplicateRecordFields #-}

data ConfigRun =
    ConfigRun {
      _debug     :: Bool,
      _inFiles   :: Set FilePath,
      _language  :: Language,
      _outFile   :: FilePath,
      _verbosity :: Integer
    }
```

We also need to write a function that does the transition.  This cannot
be automated (at least not fully), because you need to fill in the
remaining blanks after the configuration phase:

``` haskell
{-# LANGUAGE RecordWildCards #-}

adHocBuildConfig :: ConfigBuild -> ConfigRun
adHocBuildConfig ConfigBuild{..} =
    ConfigRun {
      _debug     = getAny _debug,
      _inFiles   = _inFiles,
      _language  = maybe V2 id (getLast _language),
      _outFile   = maybe "a.out" id (getLast _outFile),
      _verbosity = getSum _verbosity
    }
```

This isn't too bad, but the `ConfigRun` type duplicates so much
information, because we failed to capture the essence of what makes the
build phase different from the run phase.


Unifying phases
===============

The idea is that we need a certain piece of information during the run
phase like, for example, the language version.  That is just a value of
type `Language`.  During the build phase we need a monoid that captures
the last language version chosen as well as a "no explicit choice" case.
That happens to be `Last Language`.  We do something similar with the
verbosity:  It's just a plain `Integer` during the run phase, but during
the build phase it needs to be coupled with additivity, that is `Sum
Integer`.

More generally for each configuration field we seem to select a regular
type `A` during the run phase, but a monoidal type `F A` during the
build phase.  If we abstract over this selection, then we can unify the
two phases into a single type:

``` haskell
data Config select =
    Config {
      _debug     :: select Identity Any,
      _inFiles   :: select Identity (Set FilePath),
      _language  :: select Last Language,
      _outFile   :: select Last FilePath,
      _verbosity :: select Sum Integer
    }
    deriving (Generic)
```

For each field the `select` type is applied to an `F` and an `A`.  If it
returns `F A`, we're in the build phase, and if it returns just `A`,
we're in the run phase:

``` haskell
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

newtype Build f a = Build { fromBuild :: f a }
    deriving (Eq, Foldable, Functor, Monoid,
              Ord, Show, Traversable)

newtype Run f a = Run { fromRun :: a }
    deriving (Eq, Foldable, Functor, Monoid,
              Ord, Show, Traversable)
```

You may be asking why I enabled PolyKinds there.  The `Run` type
abstracts over `f`, but doesn't use it, so its kind is defaulted to `*`,
and we couldn't use it as an argument to `Config` (kind error).  Now we
could write a kind signature for `f`, but since we're going to need an
extension anyway, we might as well just make it kind-polymorphic
instead.

Of course during the build phase we want our configuration type to be a
monoid:

``` haskell
{-# LANGUAGE FlexibleInstances #-}

instance Monoid (Config Build) where
    mempty = memptydefault
    mappend = mappenddefault
```

All we need now is the function that switches from the build phase to
the run phase, which we'll write in terms of a little helper function.
For each field we need to decide how to get rid of the `f` layer from
the build phase, a recurring pattern:

``` haskell
(!<-) :: (f a -> b) -> Build f a -> Run f b
(!<-) f = Run . f . fromBuild

infix 1 !<-
```

The mnemonic for this function is that the arrow is pointing toward the
bang, the run phase, where stuff happens.

``` haskell
buildConfig :: Config Build -> Config Run
buildConfig Config{..} =
    Config {
      _debug     = runIdentity                !<- _debug,
      _inFiles   = runIdentity                !<- _inFiles,
      _language  = maybe V2 id . getLast      !<- _language,
      _outFile   = maybe "a.out" id . getLast !<- _outFile,
      _verbosity = getSum                     !<- _verbosity
    }
```

In most applications that's all we need.  We can now gather
configuration data from multiple sources,

``` haskell
getArgsConfig :: IO (Config Build)
getEnvConfig  :: IO (Config Build)
```

and then just combine them:

``` haskell
liftA2 (<>) getArgsConfig getEnvConfig
```


Dynamic configuration
=====================

In the above there is a piece missing.  Didn't we say that individual
files can specify extra options in a pragma?  Now this is actually a bit
of a chicken/egg problem:  At the point when we know which files we are
going to compile we have left the build phase.

In this particular application we could just attempt to read the pragma
before switching to the run phase, but that is just a precursor to bad
application design.  As soon as we start using configuration data during
the build phase, we're paving the way for complicated configuration
logic and ultimately a user interface that is hard to explain.  There
should be a definite line when we stop collecting and start using
configuration data.

The basic idea is that at any point in the application we can return to
the build phase.  In our compiler example for each file we would return
to the build phase to read the top of the file looking for the pragma,
and once either the pragma is found or we're convinced that it's not
there, we use `buildConfig` again.  The resulting configuration is now
specific to that file.

Unlike the transition to the run phase the opposite direction is
entirely mechanical.  We'll do it by hand here, but there is likely a
fully automated way using generics:

``` haskell
unbuild :: (Applicative f) => Run f a -> Build f a
unbuild = Build . pure . fromRun

unbuildConfig :: Config Run -> Config Build
unbuildConfig Config{..} =
    Config {
      _debug     = unbuild _debug,
      _inFiles   = unbuild _inFiles,
      _language  = unbuild _language,
      _outFile   = unbuild _outFile,
      _verbosity = unbuild _verbosity
    }
```

We can now return to the build phase whenever we need to accumulate more
configuration data.  This is also useful in the common case when the
application allows its users to override the path to the configuration
file using a command line option.


The meta phase
==============

Now that our configuration type abstracts over the selector type we can
think of other kinds of phases like a *meta phase*:

``` haskell
newtype Meta c f a = Meta { fromMeta :: c }
    deriving (Eq, Foldable, Functor, Monoid,
              Ord, Show, Traversable)
```

Unlike the other phases this does not include any configuration
information at all, but meta-data about each field, for example help
strings:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

helpConfig :: Config (Meta Text)
helpConfig =
    Config {
      _debug     = Meta "Debugging information",
      _inFiles   = Meta "Input files",
      _language  = Meta "Language version",
      _outFile   = Meta "Output file",
      _verbosity = Meta "Verbosity"
    }
```

You could come up with a more elaborate help type than `Text` and use it
to generate the command line synopsis that `--help` prints by using
generics.  I will leave it to your imagination to make use of this phase
for other things that are *meta*.
