{- |
   Module      : Control.Quiver.Interleave
   Description : Interleave values from multiple Quivers
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Control.Quiver.Interleave
  ( spinterleave
  ) where

import Control.Quiver
import Control.Quiver.Internal (P (..))

import Data.Either   (rights)
import Data.Function (on)
import Data.List     (sortBy)

--------------------------------------------------------------------------------

-- | Interleave the elements of the provided producers such that the
-- result consists of the values from all of them sorted by the
-- provided comparison function.
spinterleave :: (Monad f) => (b -> b -> Ordering) -> [P a a' b () f ()] -> P a a' b () f ()
spinterleave cmp ps = do
  aps <- qlift (rights <$> mapM spnext ps)
  go aps
  where
    go []  = return ()
    go aps = do let (a,p):aps' = sortBy (cmp`on`fst) aps
                emit_ a
                eap' <- qlift $ spnext p
                go (either (const aps') (:aps') eap')

-- TODO: consider having it just return a Maybe
spnext :: (Monad f) => P a a' b () f r -> f (Either r (b, P a a' b () f r))
spnext = go
  where
    go p = case p of
             Consume _ _ p' -> go p'
             Produce b pr _ -> return (Right (b, pr ()))
             Enclose f      -> f >>= go
             Deliver r      -> return (Left r)
