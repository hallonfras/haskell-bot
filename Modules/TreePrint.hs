{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = Multi-way Trees and Forests
--
-- The @'Tree' a@ type represents a lazy, possibly infinite, multi-way tree
-- (also known as a /rose tree/).
--
-- The @'Forest' a@ type represents a forest of @'Tree' a@s.
--
-----------------------------------------------------------------------------

module TreePrint(
      -- * Ascii Drawings
    treeDraw
    , drawForest

    ) where

import Data.Tree

treeDraw :: Tree String -> String
treeDraw = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "L " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)
