{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Hrefactor (module Hrefactor, module Hrefactor.Types, module Hrefactor.Comments) where

import           Control.Applicative ((<$>))
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Data
import           Data.Default
import           Data.Foldable
import           Data.Function (on)
import           Data.Functor
import           Data.Functor.Identity
import           Data.List (groupBy, intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text.IO as ST
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T hiding (singleton)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.IO as Text
import           Data.Traversable
import           GHC.Generics
import           HIndent
import           HIndent.Comments
import           HIndent.Pretty
import           HIndent.Types
import           Language.Haskell.Exts.Annotated
       hiding (Style, prettyPrint, Pretty, style, parse)
import qualified Hrefactor.Refactoring as Old
import           Hrefactor.Comments
import           Hrefactor.Styles.Exact
import           Hrefactor.Styles.TonyDay
import           Hrefactor.Types

reformat' :: Style -> Maybe [Extension] -> Text -> Either String Builder
reformat' style mexts code = 
  (prettyAst style . removeImportComments) <$>
  getParse mexts code

styles' :: [Style]
styles' = 
  styles <>
  [tonyDay def]

-- getParse :: Text -> Either String (Module NodeInfo)
getParse :: Maybe [Extension] -> Text -> Either String (Module NodeInfo)
getParse mexts code = 
  case parseModuleWithComments mode'
                               (T.unpack code) of
    ParseOk (m,comments) -> 
      Right $
      uncurry shoveCommentInModule (annotateComments m comments)
    ParseFailed _ e -> Left e
  where mode' = 
          case mexts of
            Just exts -> 
              parseMode {extensions = exts}
            Nothing -> parseMode

formatAst :: Style -> Module NodeInfo -> Builder
formatAst style ast = 
  prettyAst style (removeImportComments ast)

prettyAst :: Pretty ast
          => Style -> ast NodeInfo -> Builder
prettyAst style ast = 
  runPrinterStyle parseMode
                  style
                  (pretty ast)

-- | Pretty print the given printable thing.
runPrinterStyle :: ParseMode -> Style -> (forall s. Printer s ()) -> Builder
runPrinterStyle mode' (Style _name _author _desc st extenders config preprocessor) m = 
  maybe (error "Printer failed with mzero call.")
        psOutput
        (runIdentity 
           (runMaybeT (execStateT 
                         (runPrinter m)
                         (PrintState 0
                                     mempty
                                     False
                                     0
                                     1
                                     st
                                     extenders
                                     config
                                     False
                                     False
                                     mode'
                                     preprocessor))))

trip :: Text -> Either String Builder
trip t = 
  formatAst (tonyDay def) <$>
  getParse Nothing t

tt :: [Text]
tt = 
  ["","\n","-- comment\n","x=1","-- comment\nx=1","x::Int\nx=1"]

removeComment :: ImportDecl NodeInfo -> ImportDecl NodeInfo
removeComment (decl@(ImportDecl (NodeInfo n _) _ _ _ _ _ _ _)) = 
  decl {importAnn = NodeInfo n []}

removeImportComments :: Module NodeInfo -> Module NodeInfo
removeImportComments (Module l h p i d) = 
  Module l h p (removeComment <$> i) d
removeImportComments m = m

testFile :: FilePath -> FilePath -> IO ()
testFile fin fout = 
  do t <- Text.readFile fin
     case trip t of
       Left e -> print e
       Right s -> 
         Text.writeFile fout
                        (toLazyText s)

t1 :: IO ()
t1 = 
  testFile "testing/tests/test1.hs" "testing/tests/result1.hs"

-- to do
getParse' :: Text -> (Module SrcSpanInfo,[Comment])
getParse' t = 
  fromParseResult 
    (parseModuleWithComments parseMode
                             (T.unpack t))

testFileOrig :: FilePath -> FilePath -> IO ()
testFileOrig fin fout = 
  do t <- Text.readFile fin
     case reformat (tonyDay def) Nothing t of
       Left e -> print e
       Right s -> 
         Text.writeFile fout
                        (toLazyText s)

shoveCommentInModule :: [ComInfo] -> Module NodeInfo -> Module NodeInfo
shoveCommentInModule c (Module (NodeInfo s c') h p i d) = 
  Module (NodeInfo s (c' <> c)) h p i d
shoveCommentInModule _ m = m

tripOrig :: Text -> Either String Text
tripOrig t = 
  toLazyText <$>
  reformat (tonyDay def) Nothing t
