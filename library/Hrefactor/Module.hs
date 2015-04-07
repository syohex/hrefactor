{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC-fno-warn-name-shadowing#-}

module Hrefactor.Module where

import Control.Applicative
import Control.Monad.State.Strict hiding (state, State)
import Data.Default
import Data.List
import Data.Maybe
import HIndent.Pretty
import HIndent.Types
import Language.Haskell.Exts.Annotated hiding (Style, prettyPrint, Pretty, style, parse)

data DeclSep = DeclSepSingle
             | DeclSepDouble

-- | config state.
data ModuleConfig =
       ModuleConfig
         { _xRemoveImportComments :: Bool
         , _xDeclarationSpace :: Bool
         , _xDeclSep :: DeclSep
         }

instance Default ModuleConfig where
  def = ModuleConfig True True DeclSepDouble

userGets :: (ModuleConfig -> a) -> Printer ModuleConfig a
userGets f = gets (f . psUserState)

type Extend f = f NodeInfo -> Printer ModuleConfig ()

{-|
  Outputs 2 new lines.
  This is meant to represent a section separator.  Some sectional ideas include:
  - separation of import declarations (not yet implemented) - so that sorting can occur within sectional boundaries
  - separation of module exports into logical units, often denoted by haddock (not yet implemented)
  - separation of discrete function declaration+body 
-}
sepSection :: MonadState (PrintState s) m => ModuleConfig -> m ()
sepSection cfg =
  let sep' =
              case _xDeclSep cfg of
                DeclSepDouble -> "\n\n"
                DeclSepSingle -> "\n"
  in do
    write sep'
    modify (\s -> s { psNewline = True })

-- | changes annotations to () so that show can be used for comparisons
bare :: (Annotated ast)
     => ast a -> ast ()
bare src = void src

-- | listify's single-line multiple pragmas
listifyPragma :: ModulePragma a -> [ModulePragma a]
listifyPragma (LanguagePragma a xs) =
  fmap (\x -> LanguagePragma a [x]) xs
listifyPragma (OptionsPragma a tool str) =
  fmap (OptionsPragma a tool) (words str)
listifyPragma p@(AnnModulePragma _ _) = [p]

-- | pragma comparison (uses show in part)
comparePragmas :: (Show a)
               => ModulePragma a -> ModulePragma a -> Ordering
comparePragmas (LanguagePragma{}) (OptionsPragma{}) = LT
comparePragmas (LanguagePragma{}) (AnnModulePragma{}) = LT
comparePragmas (OptionsPragma{}) (AnnModulePragma{}) = LT
comparePragmas (OptionsPragma{}) (LanguagePragma{}) = GT
comparePragmas (AnnModulePragma{}) (LanguagePragma{}) = GT
comparePragmas (AnnModulePragma{}) (OptionsPragma{}) = GT
comparePragmas (LanguagePragma _ []) (LanguagePragma _ []) = EQ
comparePragmas (LanguagePragma _ []) (LanguagePragma{}) = LT
comparePragmas (LanguagePragma{}) (LanguagePragma _ []) = GT
comparePragmas (LanguagePragma _ (Symbol{}:_)) (LanguagePragma _ (Ident{}:_)) = LT
comparePragmas (LanguagePragma _ (Ident{}:_)) (LanguagePragma _ (Symbol{}:_)) = GT
comparePragmas (LanguagePragma _ (Ident _ n:_)) (LanguagePragma _ (Ident _ n':_)) =
  compare n n'
comparePragmas (LanguagePragma _ (Symbol _ n:_)) (LanguagePragma _ (Symbol _ n':_)) =
  compare n n'
comparePragmas (OptionsPragma _ Nothing _) (OptionsPragma _ (Just _) _) = LT
comparePragmas (OptionsPragma _ (Just _) _) (OptionsPragma _ Nothing _) = GT
comparePragmas (OptionsPragma _ Nothing s) (OptionsPragma _ Nothing s') =
  compare s s'
comparePragmas (OptionsPragma _ (Just t) s) (OptionsPragma _ (Just t') s') =
  case compare t t' of
    LT -> LT
    GT -> GT
    EQ -> compare s s'
comparePragmas (AnnModulePragma _ s) (AnnModulePragma _ s') =
  compareAnnotation s s'

-- | annotation pragma comparison
compareAnnotation :: Annotation a -> Annotation a1 -> Ordering
compareAnnotation (Ann{}) (TypeAnn{}) = LT
compareAnnotation (Ann{}) (ModuleAnn{}) = LT
compareAnnotation (ModuleAnn{}) (Ann{}) = LT
compareAnnotation (ModuleAnn{}) (TypeAnn{}) = GT
compareAnnotation (TypeAnn{}) (Ann{}) = GT
compareAnnotation (TypeAnn{}) (ModuleAnn{}) = GT
compareAnnotation (Ann _ (Ident{}) _) (Ann _ (Symbol{}) _) = LT
compareAnnotation (Ann _ (Symbol{}) _) (Ann _ (Ident{}) _) = GT
compareAnnotation (Ann _ (Ident _ s) e) (Ann _ (Ident _ s') e') =
  case compare s s' of
    LT -> LT
    GT -> GT
    EQ ->
      compare (show $ bare e) (show $ bare e')
compareAnnotation (Ann _ (Symbol _ s) e) (Ann _ (Symbol _ s') e') =
  case compare s s' of
    LT -> LT
    GT -> GT
    EQ ->
      compare (show $ bare e) (show $ bare e')
compareAnnotation (TypeAnn _ (Ident{}) _) (TypeAnn _ (Symbol{}) _) = LT
compareAnnotation (TypeAnn _ (Symbol{}) _) (TypeAnn _ (Ident{}) _) = GT
compareAnnotation (TypeAnn _ (Ident _ s) e) (TypeAnn _ (Ident _ s') e') =
  case compare s s' of
    LT -> LT
    GT -> GT
    EQ ->
      compare (show $ bare e) (show $ bare e')
compareAnnotation (TypeAnn _ (Symbol _ s) e) (TypeAnn _ (Symbol _ s') e') =
  case compare s s' of
    LT -> LT
    GT -> GT
    EQ ->
      compare (show $ bare e) (show $ bare e')
compareAnnotation (ModuleAnn _ e) (ModuleAnn _ e') =
  compare (show $ bare e) (show $ bare e')

-- | pragma equality
eqPragma :: (Show a)
         => ModulePragma a -> ModulePragma a -> Bool
eqPragma x x' =
  case comparePragmas x x' of
    EQ -> True
    _  -> False

-- | sorts and removes duplicate pragmas
nubPragmas :: (Show a)
           => [ModulePragma a] -> [ModulePragma a]
nubPragmas = fmap head . groupBy eqPragma . sortBy comparePragmas

-- | import declaration comparison (uses show in part)
compareImports :: ImportDecl a -> ImportDecl a -> Ordering
compareImports (ImportDecl _ (ModuleName _ name) q src safe pkg as specs) (ImportDecl _ (ModuleName _ name') q' src' safe' pkg' as' specs') =
  case compare name name' of
    GT -> GT
    LT -> LT
    EQ ->
      case compare q q' of
        GT -> GT
        LT -> LT
        EQ ->
          case (specs, specs') of
            (Nothing, Just _) -> LT
            (Just _, Nothing) -> GT
            _ ->
              case compare
                     (show $
                        fmap bare specs)
                     (show $
                        fmap bare specs') of
                GT -> GT
                LT -> LT
                EQ ->
                  case compare
                         (show $
                            fmap bare as)
                         (show $
                            fmap bare as') of
                    GT -> LT
                    LT -> GT
                    EQ ->
                      compare (q, src, safe, pkg) (q', src', safe', pkg')

-- | import declaration equality
eqImports :: (Show a)
          => ImportDecl a -> ImportDecl a -> Bool
eqImports x x' =
  case compareImports x x' of
    EQ -> True
    _  -> False

-- | sorts imports and removes duplicates
nubImports :: (Show a)
           => [ImportDecl a] -> [ImportDecl a]
nubImports = fmap head . groupBy eqImports . sortBy compareImports

-- | cleans up pragmas and imports
clean :: (Show a)
      => Module a -> Module a
clean (Module a mayModHead pragmas imps decls) =
  Module a mayModHead pragmas' imps' decls
  where
    pragmas' =
      nubPragmas $
        concat (fmap listifyPragma pragmas)
    imps' = nubImports imps
clean m = m

-- | cleans up a module, and opinionated macro-format
prettyModule :: MonadState (PrintState s) m => ModuleConfig -> Module NodeInfo -> m ()
prettyModule cfg x =
  let x' = clean x
  in case x' of
    Module _ mayModHead pragmas imps decls ->
      do
        inter newline (fmap pretty pragmas)
        unless (null pragmas || isNothing mayModHead) (sepSection cfg)
        case mayModHead of
          Nothing -> return ()
          Just modHead ->
            do
              pretty modHead
              unless (null imps) (sepSection cfg)
        inter newline (fmap pretty imps)
        unless (null imps || null decls) (sepSection cfg)
        prettyDecls (sepSection cfg) decls
    XmlPage{} ->
      error "FIXME: No implementation for XmlPage."
    XmlHybrid{} ->
      error "FIXME: No implementation for XmlHybrid."

-- | Print all the declarations separated by sep
prettyDecls :: MonadState (PrintState s) m
            => m () -> [Decl NodeInfo] -> m ()
prettyDecls sep decls =
  do
    sequence_ $
      (\x -> do
         pretty x
         if notSig x
           then sep
           else newline) <$>
      init decls
    pretty (last decls)

  where
    notSig (TypeSig{}) = False
    notSig _ = True
