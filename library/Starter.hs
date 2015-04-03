{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Starter where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Foldable
import Data.Functor
import Data.Traversable
import Data.Data
import Data.Default
import GHC.Generics

data StarterSum = StarterSum1 | StarterSum2 deriving (Show, Eq, Typeable, Data, Generic)

data Starters a = Starters { _starterA :: a, _starterB :: a } deriving (Eq, Show, Foldable, Traversable)

instance Functor Starters where
  fmap f (Starters a b) = Starters (f a) (f b)

instance Applicative Starters where
  pure a = Starters a a
  Starters f g <*> Starters a b = Starters (f a) (g b)

data StarterRecord a b = StarterRecord {_starter1 :: a, _starter2 ::b } deriving (Show, Eq, Typeable, Data, Generic)

makeLenses ''StarterRecord

data StarterConfig = StarterConfig
  { _starterVerbose :: Bool
  } deriving (Show, Eq, Typeable, Data, Generic)

instance Default StarterConfig where
    def = StarterConfig True

main :: IO ()
main = undefined
