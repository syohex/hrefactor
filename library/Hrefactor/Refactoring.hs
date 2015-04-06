{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Hrefactor.Refactoring where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Functor.Identity
import           Data.List
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as Text
import           Data.Traversable
import           HIndent
import           HIndent.Comments
import           HIndent.Pretty
import           HIndent.Types
import           Language.Haskell.Exts.Annotated
       hiding (Style, prettyPrint, Pretty, style, parse)
import           Language.Haskell.HLint3
import           Prelude
import           Text.PrettyPrint.HughesPJ (Doc)
import           Text.Show.Pretty hiding (Con)
import           Hrefactor.Comments
import           Hrefactor.Styles.Exact hiding (State)
import           Hrefactor.Styles.TonyDay hiding (State)

-- helpers
getParse :: Text.Text -> (Module SrcSpanInfo,[Comment])
getParse t = 
  fromParseResult 
    (parseModuleWithComments parseMode
                             (Text.unpack t))

getAst :: Text.Text -> Module NodeInfo
getAst = 
  uncurry (placeComments LowestLayer) .
  getParse

getAst' :: Attachment -> Text.Text -> Module NodeInfo
getAst' layer = 
  uncurry (placeComments layer) .
  getParse

getAstOld :: Text.Text -> ([ComInfo],Module NodeInfo)
getAstOld = uncurry annotateComments . getParse

getComments :: Functor f
            => f NodeInfo -> f [ComInfo]
getComments ast = 
  fmap (\(NodeInfo _ cs) -> cs) ast

getPretty :: Style -> Text.Text -> Text.Text
getPretty style = Text.toLazyText . prettyAst style . getAst

getPretty' :: Attachment -> Style -> Text.Text -> Text.Text
getPretty' att style = Text.toLazyText . prettyAst style . getAst' att

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

prettyAst :: Pretty ast
          => Style -> ast NodeInfo -> Text.Builder
prettyAst style ast = 
  runPrinterStyle parseMode
                  style
                  (pretty ast)

prettyAst' :: (Pretty ast)
           => Style
           -> (ast NodeInfo -> (forall s. Printer s ()))
           -> ast NodeInfo
           -> Text.Builder
prettyAst' style p ast = 
  runPrinterStyle parseMode
                  style
                  (p ast)

toList :: Traversable t
       => t t1 -> [t1]
toList = 
  reverse .
  flip execState [] .
  traverse (\x -> modify (\xs -> x : xs))

posn :: Functor f
     => f NodeInfo -> f ((Int,Int),(Int,Int),[ComInfo])
posn ast = 
  fmap (\(NodeInfo (SrcSpanInfo (SrcSpan _ l0 c0 l1 c1) _) cs) -> 
          ((l0,c0),(l1,c1),cs))
       ast

show' :: (Show a)
      => a -> Doc
show' = ppDoc

showText :: Text.Text -> IO ()
showText t = Text.putStr t

commentLoc :: Attachment -> Text.Text -> [Int]
commentLoc att = 
  findIndices (not . null) .
  Hrefactor.Refactoring.toList .
  getComments .
  getAst' att

-- hlint
getDeclsIdea :: Text.Text -> IO [Idea]
getDeclsIdea t = 
  do (flags,classify,hint) <- autoSettings
     let (m@(Module _ _ _ _ decls),c) = getParse t
         scope = scopeCreate m
     return $ concat $
       hintDecl hint scope m <$>
       decls

getIdeas :: Text.Text -> IO [Idea]
getIdeas t = 
  do (flags,classify,hint) <- autoSettings
     let (m,c) = getParse t
     return $
       applyHints classify
                  hint
                  [(m,c)]

-- testing
ts :: [Text]
ts = 
  ["x = putStrLn . show --end\n"
  ,"{-1-}x\n"
  ,"x{-1-}"
  ,"{-1-} x1 = putStrLn {-2-} . show {-3-}\n"]

testCommentLoc :: Bool
testCommentLoc = 
  commentLoc LowestLayer
             (ts !! 0) ==
  [14] &&
  commentLoc HighestLayer
             (ts !! 0) ==
  [1]

exactOk' :: Text.Text -> Bool
exactOk' t = 
  t ==
  getPretty' LowestLayer exact t &&
  t ==
  getPretty' HighestLayer exact t

exactOk :: Text.Text -> Bool
exactOk t = 
  t ==
  getPretty exact t

testExact :: [Bool]
testExact = exactOk <$> ts

{- 
difficulties with using HighestLayer

Processes either pretty print the comments, then pretty print the ast element, or vice versa.  No concept exists of printing comments inside an ast element.

eg
λ> getPretty exact LowestLayer "x1 = putStrLn {-1-} . {-2-} show {-3-}\n"
"x1 = putStrLn {-1-} . {-2-} show {-3-}\n"
λ> getPretty exact HighestLayer "x1 = putStrLn {-1-} . {-2-} show {-3-}\n"
"x1 = putStrLn       .       show {-3-}{-1-}   {-2-}\n"

-}
{-
comment-only tests
λ> show' $ getComments $ getAst ""
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "\n"
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "-- 1"
Module
  [ ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 1 1 1 5) " 1"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []


λ> show' $ getComments $ getAst ""
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "\n"
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "-- 1"
Module
  [ ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 1 1 1 5) " 1"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []
λ> show' $ getComments $ getAst "-- 1\n-- 2"
Module
  [ ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 1 1 1 5) " 1"
      , comInfoLocation = Just Before
      }
  , ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 2 1 2 5) " 2"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []
λ> show' $ getComments $ getAst "{-1-}{-2-}"
Module
  [ ComInfo
      { comInfoComment =
          Comment True (SrcSpan "<unknown>.hs" 1 1 1 6) "1"
      , comInfoLocation = Just After
      }
  , ComInfo
      { comInfoComment =
          Comment True (SrcSpan "<unknown>.hs" 1 6 1 11) "2"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []
λ> 
-}
{-
comment tests

λ> show' $ getComments $ getAst "x -- 1"
Module
  []
  Nothing
  []
  []
  [ SpliceDecl
      []
      (Var
         []
         (UnQual
            []
            (Ident
               [ ComInfo
                   { comInfoComment =
                       Comment False (SrcSpan "<unknown>.hs" 1 3 1 7) " 1"
                   , comInfoLocation = Just After
                   }
               ]
               "x")))
  ]


Without the '\n', the Module happens to be the same srcSpan as any of the other options, so that the first comment attaches to the module rather than the decl

λ> show' $ posnT' $ getAst "{-1-}x{-2-}\n"
Module
  ( ( 1 , 6 ) , ( 2 , 0 ) , [] )
  Nothing
  []
  []
  [ SpliceDecl
      ( ( 1 , 6 ) , ( 1 , 7 ) , [] )
      (Var
         ( ( 1 , 6 ) , ( 1 , 7 ) , [] )
         (UnQual
            ( ( 1 , 6 ) , ( 1 , 7 ) , [] )
            (Ident
               ( ( 1 , 6 )
               , ( 1 , 7 )
               , [ ComInfo
                     { comInfoComment =
                         Comment True (SrcSpan "<unknown>.hs" 1 1 1 6) "1"
                     , comInfoLocation = Just Before
                     }
                 , ComInfo
                     { comInfoComment =
                         Comment True (SrcSpan "<unknown>.hs" 1 7 1 12) "2"
                     , comInfoLocation = Just After
                     }
                 ]
               )
               "x")))
  ]
λ> 

-}
var' :: ComInfoLocation -> Text -> Exp NodeInfo
var' place att = 
  let cloc = 
        if place == Before
           then SrcSpan "" 1 1 1 6
           else SrcSpan "" 1 2 1 7
      sloc = 
        if place == Before
           then SrcSpan "" 1 6 1 7
           else SrcSpan "" 1 1 1 2
      c = 
        [ComInfo {comInfoComment = 
                    Comment True cloc "1"
                 ,comInfoLocation = Just place}]
  in Var NodeInfo {nodeInfoSpan = 
                     SrcSpanInfo {srcInfoSpan = sloc
                                 ,srcInfoPoints = []}
                  ,nodeInfoComments = 
                     if att == "Var"
                        then c
                        else []}
         (UnQual NodeInfo {nodeInfoSpan = 
                             SrcSpanInfo {srcInfoSpan = sloc
                                         ,srcInfoPoints = []}
                          ,nodeInfoComments = 
                             if att == "UnQual"
                                then c
                                else []}
                 (Ident NodeInfo {nodeInfoSpan = 
                                    SrcSpanInfo {srcInfoSpan = sloc
                                                ,srcInfoPoints = []}
                                 ,nodeInfoComments = 
                                    if att == "Ident"
                                       then c
                                       else []}
                        "x"))

{-
testVar' :: IO ()
testVar' =
  showText $ Text.toLazyText $ mconcat 
  [ "-- " 
  <> Text.fromText (styleName s) 
  <> " " 
  <> (Text.fromLazyText . Text.pack . show) l 
  <> " " 
  <> Text.fromLazyText t 
  <> ":\n" 
  <> prettyAst s (var' l t)
  <> "\n"
  | s <- styles
  , l <- [Before,After]
  , t <- ["Var","UnQual","Ident"] :: [Text]
  ]
-}
-- module merging
-- | sort of a mappend anyway
maybeMappend :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeMappend ma x x' = 
  case x of
    Nothing -> 
      case x' of
        Nothing -> Nothing
        Just v' -> Just v'
    Just v -> 
      case x' of
        Nothing -> Just v
        Just v' -> Just (ma v v')

-- | favours the first module header, but merges the export list (but no duplicate check)
mergeModuleHead :: ModuleHead a -> ModuleHead a -> ModuleHead a
mergeModuleHead (ModuleHead a name' warn exports) (ModuleHead _ _ warn' exports') = 
  ModuleHead 
    a
    name'
    (maybeMappend const warn warn')
    (maybeMappend 
       (\(ExportSpecList a' es) (ExportSpecList _ es') -> 
          ExportSpecList a'
                         (es <> es'))
       exports
       exports')

-- | full merge of two modules
mergeModule :: (Show a)
            => Module a -> Module a -> Module a
mergeModule (Module a h pragmas imps decls) (Module _ h' pragmas' imps' decls') = 
  clean (Module a
                (maybeMappend mergeModuleHead h h')
                (pragmas <> pragmas')
                (imps <> imps')
                (decls <> decls'))
mergeModule x0 _ = x0

testMerge :: FilePath -> FilePath -> IO ()
testMerge f f' = 
  do t <- Text.readFile f
     t' <- Text.readFile f'
     let ast1 = testAst t
         ast2 = testAst t'
         ast = 
           case (ast1,ast2) of
             (Left e1,Left e2) -> Left (e1 ++ e2)
             (Left e1,Right _) -> Left e1
             (Right _,Left e2) -> Left e2
             (Right (cominfo1,m1),Right (cominfo2,m2)) -> 
               Right (cominfo1 ++ cominfo2,m1 `mergeModule` m2)
     case ast of
       Left _ -> return ()
       Right (cominfo,m) -> 
         let p = prettyAst (tonyDay def) m
         in Text.putStr $ Text.toLazyText p

order :: (Traversable t,Num s)
      => t a -> (t s,s)
order ast = 
  runState (traverse (\x -> 
                        do c <- get
                           modify (+1)
                           return c)
                     ast)
           0

showT :: (Traversable t,Show a)
      => t a -> [String]
showT ast = 
  execState (traverse (\x -> 
                         modify (\xs -> show x : xs))
                      ast)
            []

incT :: Module a -> Module Int
incT ast = 
  flip evalState 0 $
  traverse inc ast

inc :: t -> State Int Int
inc _ = 
  do modify (+ 1)
     get

-- posnT :: (Traversable t, Typeable a) => t a -> Int
posnT :: Module SrcSpanInfo -> Module ((Int,Int),(Int,Int))
posnT ast = 
  fmap (\(SrcSpanInfo (SrcSpan _ l0 c0 l1 c1) _) -> 
          ((l0,c0),(l1,c1)))
       ast

posnT' :: Functor f
       => f NodeInfo -> f ((Int,Int),(Int,Int),[ComInfo])
posnT' ast = 
  fmap (\(NodeInfo (SrcSpanInfo (SrcSpan _ l0 c0 l1 c1) _) cs) -> 
          ((l0,c0),(l1,c1),cs))
       ast
