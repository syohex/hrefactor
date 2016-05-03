{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC-fno-warn-type-defaults#-}

module Hrefactor where

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Functor
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T hiding (singleton)
import           Data.Text.Lazy.Builder
import           HIndent (parseMode)
import           HIndent.Comments (annotateComments)
import           HIndent.Types
import           Hrefactor.Module
import           Language.Haskell.Exts.Annotated hiding (Style, prettyPrint, Pretty, style, parse)

reformat' :: Style -> Maybe [Extension] -> Text -> Either String Builder
reformat' style mexts code =
  (runPrettyModule style . removeImportComments) <$>
  getParse mexts code

runPrettyModule :: Style -> Module NodeInfo -> Builder
runPrettyModule style ast =
  runPrinterStyle parseMode style (prettyModule def ast)

getParse :: Maybe [Extension] -> Text -> Either String (Module NodeInfo)
getParse mexts code =
  case parseModuleWithComments mode' (T.unpack code) of
    ParseOk (m, comments) ->
      Right $
        uncurry shoveCommentInModule (annotateComments m comments)
    ParseFailed _ e -> Left e
  where
    mode' =
      case mexts of
        Just exts ->
          parseMode { extensions = exts }
        Nothing -> parseMode

shoveCommentInModule :: [ComInfo] -> Module NodeInfo -> Module NodeInfo
shoveCommentInModule c (Module (NodeInfo s c') h p i d) =
  Module (NodeInfo s (c' <> c)) h p i d
shoveCommentInModule _ m = m

-- | Pretty print the given printable thing.
runPrinterStyle :: ParseMode -> Style -> (forall s. Printer s ()) -> Builder
runPrinterStyle mode' (Style _name _author _desc st extenders config preprocessor) m =
  maybe (error "Printer failed with mzero call.") psOutput
    (runIdentity
       (runMaybeT
          (execStateT (runPrinter m)
             (PrintState 0 mempty False 0 1 st extenders config False False mode' preprocessor))))

removeImportComments :: Module NodeInfo -> Module NodeInfo
removeImportComments (Module l h p i d) =
  Module l h p (removeComment <$> i) d
removeImportComments m = m

removeComment :: ImportDecl NodeInfo -> ImportDecl NodeInfo
removeComment (decl@(ImportDecl (NodeInfo n _) _ _ _ _ _ _ _)) =
  decl { importAnn = NodeInfo n [] }
