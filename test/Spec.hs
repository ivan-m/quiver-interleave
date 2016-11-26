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
  describe "interleave" $ do
    prop "same as list-based" $
      forAllShrink orderedSubLists shrink $ \ass ->
        interleaveSort ass == sort (concat ass)
    it "propagates errors" $
      spIdentity (spinterleave compare [spevery [1,4,7]
                                       ,spevery [2,5,8]
                                       ,3 >:> spfailed "failed"]
                  >->> spToList)
        == (SPFailed "failed", [1,2,3] :: [Int]) -- The error and the values found before the error came up.

spToList :: SQ a x f [a]
spToList = spfoldr (:) []

spIdentity :: SQ a b Identity c -> c
spIdentity q = runIdentity (sprun q)

-- Assumes each sub-list is ordered.
interleaveSort :: (Ord a) => [[a]] -> [a]
interleaveSort ass = spIdentity (spinterleave compare (map spevery ass) >->> spToList >&> snd)

-- Each sub-list is ordered
orderedSubLists :: Gen [[Int]]
orderedSubLists = listOf orderedList
