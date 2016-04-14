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

import Control.Quiver.Internal (P (..))
import Control.Quiver.SP

import Data.Bool     (bool)
import Data.Either   (partitionEithers)
import Data.Function (on)
import Data.List     (sortBy)

--------------------------------------------------------------------------------

-- | Interleave the elements of the provided producers such that the
-- result consists of the values from all of them merged in by the
-- provided comparison function.
--
-- That is, if each provided Quiver returns a sequence of ordered
-- elements, then this would be the same as obtaining all the elements
-- and sorting them.
--
-- If any provided Quiver results in anything except 'SPComplete' then
-- entire stream halts, propagating the error.
spinterleave :: (Monad f) => (b -> b -> Ordering) -> [P a a' b () f (SPResult e)] -> P a a' b () f (SPResult e)
spinterleave cmp ps = do
  (errs, aps) <- qlift (partitionEithers <$> mapM spnext ps)
  case filter isErr errs of
    (e:_) -> deliver e
    _     -> go aps
  where
    go []  = spcomplete
    go aps = do let (a,p):aps' = sortBy (cmp`on`fst) aps
                emit_ a
                eap' <- qlift $ spnext p
                either (\e -> bool (go aps') (deliver e) (isErr e))
                       (go . (:aps'))
                       eap'

    isErr SPComplete = False
    isErr _          = True

spnext :: (Monad f) => P a a' b () f r -> f (Either r (b, P a a' b () f r))
spnext = go
  where
    go p = case p of
             Consume _ _ p' -> go p'
             Produce b pr _ -> return (Right (b, pr ()))
             Enclose f      -> f >>= go
             Deliver r      -> return (Left r)
