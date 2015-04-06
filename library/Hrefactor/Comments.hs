{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Comment handling.
module Hrefactor.Comments where

import Control.Monad.State.Strict
import Data.Data
import Data.Traversable
import HIndent.Types
import Language.Haskell.Exts.Annotated
       hiding (Style, prettyPrint, Pretty, style, parse)

{-
refactor of annotateComments

srcSpan's are often the same, and so the correct node to attach a comment to requires the application of an Id to each node when searching for the best node.

Two nodess can also be equally valid recipients of a comment, so there is some stylistic choice as to which node to attach to.

The attachment data type was an attempt to define this choice - LowestLayer means to choose the smallest srcSpan node amongst those equally close to the comment, and HighestLayer vice-versa. 

-}
-- data Direction = Before | After
data Attachment
  = HighestLayer 
  | LowestLayer 
  deriving (Eq)

-- | there is a variety of ways two srcSpans can be located, in addition to Before and After 
data Location
  = Before' 
  | After' 
  | Inside 
  | Wrapping 
  | Overlapping 
  | Same 
  deriving (Eq)

okDirection :: Location -> ComInfoLocation -> Bool
okDirection l d = 
  (l == Before' && d == Before) ||
  (l == After' && d == After)

-- | Where is a comment relative to a node?
commentLocation :: Comment -> NodeInfo -> Location
commentLocation (Comment _ c _) (NodeInfo (SrcSpanInfo n _) _) = 
  location c n

-- | the location of the first span in terms of the second
location :: SrcSpan -> SrcSpan -> Location
location first second
-- note that srcSpanEnd column number is +1 versus last token, thus <= & >=
  | srcSpanEnd first <= srcSpanStart second = Before'
  | srcSpanStart first >= srcSpanEnd second = After'
  | srcSpanStart first == srcSpanStart second && srcSpanEnd first ==
      srcSpanEnd second = Same
  | srcSpanStart first >= srcSpanStart second && srcSpanEnd first <=
      srcSpanEnd second = Inside
  | srcSpanStart first <= srcSpanStart second && srcSpanEnd first >=
      srcSpanEnd second = Wrapping
  | otherwise = Overlapping

-- | starting with a reference span, is the first span better than the second
-- for attaching a comment (closer, the right direction and the right shape)?
better :: Attachment -> ComInfoLocation -> SrcSpan -> SrcSpan -> SrcSpan -> Bool
better att dir ref first second
  | not (okDirection (location ref second)
                     dir) = True
  | not (okDirection (location ref first)
                     dir) = False
  | srcSpanEnd first `diff` srcSpanStart ref < srcSpanEnd second `diff`
      srcSpanStart ref = dir == Before
  | otherwise = 
    case location first second of
      Inside -> att == LowestLayer
      Wrapping -> att == HighestLayer
      Same -> att == HighestLayer
      _ -> True
  where diff :: (Int,Int) -> (Int,Int) -> (Int,Int)
        diff (a,b) (c,d) = (a - c,b - d)

-- | adds an id to each node, to enable later specific identification
addId :: (Traversable ast)
      => ast a -> ast (a,Int)
addId ast = evalState (traverse id' ast) 1
  where id' x = 
          do n <- get
             modify (+ 1)
             return (x,n)

removeId :: (Traversable ast)
         => ast (a,b) -> ast a
removeId ast = fmap fst ast

-- | find the id of the best node for the comment
findBest :: (Traversable ast)
         => Attachment
         -> ComInfoLocation
         -> Comment
         -> ast (NodeInfo,Int)
         -> Maybe (NodeInfo,Int)
findBest att dir (Comment _ c _) ast = 
  execState (traverse (\x' -> 
                         modify (`maybeBetter` x'))
                      ast)
            Nothing
  where maybeBetter Nothing x = Just x
        maybeBetter (Just x0@(NodeInfo (SrcSpanInfo n0 _) _,_)) x1@(NodeInfo (SrcSpanInfo n1 _) _,_) = 
          Just (if better att dir c n0 n1
                   then x0
                   else x1)

-- | insert comment into the node with a specific id
insertById :: (Traversable ast)
           => ComInfo -> Int -> ast (NodeInfo,Int) -> ast (NodeInfo,Int)
insertById comment n ast = 
  fmap (\a@(NodeInfo s comments,x) -> 
          if x == n
             then (NodeInfo s (comment : comments),x)
             else a)
       ast

-- | find the right node for a single comment and insert it
insertComment :: Traversable ast
              => Attachment
              -> ComInfoLocation
              -> Comment
              -> ast NodeInfo
              -> ast NodeInfo
insertComment att dir comment ast = 
  let ast' = addId ast
      best = findBest att dir comment ast'
  in case best of
       Nothing -> ast
       Just (_,b) -> 
         removeId (insertById (ComInfo comment (Just dir))
                              b
                              ast')

-- | place all comments into the ast
placeComments :: forall ast. 
                 (Data (ast NodeInfo),Traversable ast,Annotated ast)
              => Attachment -> ast SrcSpanInfo -> [Comment] -> ast NodeInfo
placeComments att ast comments = 
  let ast' = fmap (\n -> NodeInfo n []) ast
      -- Make sure to process comments top to bottom.
      reversed = reverse comments
      -- Add all comments to the ast.
      ast'' = 
        foldr (placeComment att) ast' reversed
  in 
     -- Reverse order of comments at each node.
     fmap 
       (\(NodeInfo n cs) -> 
          NodeInfo n $
          reverse cs)
       ast''

-- | place a single comment into the ast 
placeComment :: (Traversable ast)
             => Attachment -> Comment -> ast NodeInfo -> ast NodeInfo
placeComment att c@(Comment _ cspan _) ast = 
  -- Try to find the best node before this comment
  case findBest att After c (addId ast) of
    -- no Node exists before this comment. Place it with the node after
    Nothing -> insertComment att Before c ast
    -- A node before the comment exists
    Just (NodeInfo n coms,_)
    -- If it's on the same line, or 
    -- it's on a different line than the node, but the node has an
    -- EOL comment, and the EOL comment and this comment are aligned,
    -- attach this comment to the preceding node.
      | sameLine ||
          (not sameLine && alignedWithPrevious) -> 
        insertComment att After c ast
      |
      -- otherwise attach to the next node
        otherwise -> 
        insertComment att Before c ast
      where sameLine = 
              srcSpanStartLine cspan ==
              srcSpanEndLine (srcInfoSpan n)
            alignedWithPrevious
              | null coms = False
              | otherwise = 
                case last coms of
                  -- Require single line comment after the node.
                  ComInfo (Comment False prevSpan _) (Just After) -> srcSpanStartLine prevSpan ==
                                                                       srcSpanStartLine cspan -
                                                                       1 &&
                                                                       srcSpanStartColumn prevSpan ==
                                                                       srcSpanStartColumn cspan
                  _ -> False
