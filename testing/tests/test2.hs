{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- comment between pragmas and module, typically a general explanation about the module
module Test1 where

import Control.Monad


import Control.Monad (unless)
import Control.Monad (when)
import qualified Data.Foldable as F
import Data.Monoid
import HIndent
import qualified HIndent
import qualified HIndent as H
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated
       hiding (Style, prettyPrint, Pretty, style, parse)
import Hrefactor.Styles.TonyDay

-- comment floating between import list and declarations
-- comment before decl1
x1 :: Int
x1 = 1

-- comment before decl2
x2 :: String
x2 = "a"-- trailing comment, which could be interpreted as a trailer for the last decl, or could be a more general comment
