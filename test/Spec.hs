{-# LANGUAGE RankNTypes #-}
{- |
   Module      : Main
   Description : Property tests
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main (main) where

import Control.Quiver.Interleave

import Control.Quiver.SP
import Data.Functor.Identity
import Data.List             (sort)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $
  describe "interleave" $
    prop "same as list-based" $
      forAllShrink orderedSubLists shrink $ \ass ->
        interleaveSort ass == sort (concat ass)

spToList :: SQ a x f [a]
spToList = spfoldr (:) []

spIdentity :: SQ a b Identity c -> c
spIdentity = runIdentity . sprun

-- Assumes each sub-list is ordered.
interleaveSort :: (Ord a) => [[a]] -> [a]
interleaveSort ass = spIdentity (spinterleave compare (map spevery ass) >->> spToList >&> snd)

-- Each sub-list is ordered
orderedSubLists :: Gen [[Int]]
orderedSubLists = listOf orderedList
