{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestHrefactor where

import Hrefactor

import           HIndent
import Control.Applicative
import Control.Monad
import Test.Tasty.Hspec
import           Language.Haskell.Exts.Annotated hiding (Style)
import           HIndent.Types
import Data.Text.Lazy as Text
import Data.Text.Lazy.IO as Text
import           Data.Text.Lazy.Builder

tests :: IO (SpecWith())
tests =
  return $ describe "Hrefactor" $
    it "start here!" True -- testSomething `shouldReturn` True

m0 :: Module NodeInfo
m0 = 
  Module NodeInfo {nodeInfoSpan = 
                       SrcSpanInfo {srcInfoSpan = 
                                      SrcSpan "<unknown>.hs" 1 1 1 1
                                   ,srcInfoPoints = 
                                      [SrcSpan "<unknown>.hs" 1 1 1 1
                                      ,SrcSpan "<unknown>.hs" 1 1 1 1
                                      ,SrcSpan "<unknown>.hs" 1 1 1 1
                                      ,SrcSpan "<unknown>.hs" 1 1 1 1]}
                    ,nodeInfoComments = []}
          Nothing
          []
          []
          []

tt :: [Text]
tt = 
  ["","\n","-- comment\n","x=1","-- comment\nx=1","x::Int\nx=1"]

testFile :: Style -> FilePath -> FilePath -> IO ()
testFile style' fin fout = 
  do t <- Text.readFile fin
     case reformat' style' Nothing t of
       Left e -> print e
       Right s -> 
         Text.writeFile fout
                        (toLazyText s)

tGib :: IO ()
tGib = testFile gibiansky "testing/tests/test1.hs" "testing/tests/result1.hs"
