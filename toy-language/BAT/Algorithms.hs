-- |
-- Generic algorithms for BAT
module BAT.Algorithms (
    toposort
  ) where

import Control.Monad.State.Strict
import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)


-- | Topologically sort DAG. If graph contain 
toposort
  :: Ord a
  => Set a                      -- ^ Set of all nodes
  -> Map a (Set a)              -- ^ Outgoing (!) edges from given node.
  -> Maybe [a]
toposort nodes edges
  = fmap snd
  $ flip execStateT (nodes, [])
  $ forM_ nodes (toposortWorker edges Set.empty)

toposortWorker :: Ord a => Map a (Set a) -> Set a -> a -> StateT (Set a, [a]) Maybe ()
toposortWorker edges stack n = do
  -- Not a DAG
  when (n `Set.member` stack) $ lift Nothing
  -- Already visited
  unsorted <- gets fst
  when (n `Set.member` unsorted) $ do
    case n `Map.lookup` edges of
      Nothing    -> return ()
      Just descs -> forM_ descs $ toposortWorker edges (Set.insert n stack)
    (ns,sorted) <- get
    put (Set.delete n ns, n:sorted)
