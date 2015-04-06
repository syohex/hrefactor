{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestHrefactor where

import Control.Applicative
import Control.Monad
import Test.Tasty.Hspec

tests :: IO (SpecWith())
tests =
  return $ describe "Hrefactor" $
    it "start here!" True -- testSomething `shouldReturn` True
