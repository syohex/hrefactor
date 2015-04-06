-- top comment, before pragmas


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- comment between pragmas and module, typically a general explanation about the module


module Test1 where


import qualified HIndent
import HIndent
import qualified HIndent as H
import Hrefactor.Styles.TonyDay
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Exts.Annotated 
  hiding (Style,prettyPrint,Pretty,style,parse)
import Control.Monad
import Control.Monad (unless)
import Control.Monad (when)

import qualified Data.Foldable as F
-- import Control.Lens hiding (each)
-- import Data.List
import Data.Monoid

-- comment floating between import list and declarations

-- comment before decl1
x1::Int
x1=1

-- comment before decl2
x2::String
x2="a"
-- trailing comment, which could be interpreted as a trailer for the last decl, or could be a more general comment

