{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Hrefactor.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Data
import           Data.Default
import           Data.Foldable
import           Data.Functor
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as Text
import           Data.Traversable
import           GHC.Generics
import           HIndent

data DeclSep
  = DeclSepSingle 
  | DeclSepDouble 
  deriving (Show,Eq,Typeable,Data,Generic)
