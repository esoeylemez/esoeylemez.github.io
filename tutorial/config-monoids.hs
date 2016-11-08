-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Functor.Identity
import Data.Set (Set)
import Data.Text (Text)
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


data ConfigRun =
    ConfigRun {
      _debug     :: Bool,
      _inFiles   :: Set FilePath,
      _language  :: Language,
      _outFile   :: FilePath,
      _verbosity :: Integer
    }


adHocBuildConfig :: ConfigBuild -> ConfigRun
adHocBuildConfig ConfigBuild{..} =
    ConfigRun {
      _debug     = getAny _debug,
      _inFiles   = _inFiles,
      _language  = maybe V2 id (getLast _language),
      _outFile   = maybe "a.out" id (getLast _outFile),
      _verbosity = getSum _verbosity
    }


data Config select =
    Config {
      _debug     :: select Identity Any,
      _inFiles   :: select Identity (Set FilePath),
      _language  :: select Last Language,
      _outFile   :: select Last FilePath,
      _verbosity :: select Sum Integer
    }
    deriving (Generic)

instance Monoid (Config Build) where
    mempty = memptydefault
    mappend = mappenddefault


newtype Build f a = Build { fromBuild :: f a }
    deriving (Eq, Foldable, Functor, Monoid,
              Ord, Show, Traversable)

newtype Run f a = Run { fromRun :: a }
    deriving (Eq, Foldable, Functor, Monoid,
              Ord, Show, Traversable)


(!<-) :: (f a -> b) -> Build f a -> Run f b
(!<-) f = Run . f . fromBuild

infix 1 !<-


buildConfig :: Config Build -> Config Run
buildConfig Config{..} =
    Config {
      _debug     = runIdentity                !<- _debug,
      _inFiles   = runIdentity                !<- _inFiles,
      _language  = maybe V2 id . getLast      !<- _language,
      _outFile   = maybe "a.out" id . getLast !<- _outFile,
      _verbosity = getSum                     !<- _verbosity
    }


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


newtype Meta c f a = Meta { fromMeta :: c }
    deriving (Eq, Foldable, Functor, Monoid,
              Ord, Show, Traversable)


helpConfig :: Config (Meta Text)
helpConfig =
    Config {
      _debug     = Meta "Debugging information",
      _inFiles   = Meta "Input files",
      _language  = Meta "Language version",
      _outFile   = Meta "Output file",
      _verbosity = Meta "Verbosity"
    }
