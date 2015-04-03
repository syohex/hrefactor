{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestStarter where

import           Control.Applicative
import           Control.Monad
import           Test.Tasty.Hspec

tests :: IO (SpecWith())
tests =
  return $ describe "Starter" $ do
    it "start here!"     $ True -- testSomething `shouldReturn` True
