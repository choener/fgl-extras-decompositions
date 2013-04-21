{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Ear decomposition of a graph.

module Data.Graph.Inductive.Query.Ear where

import Data.Function
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Tree
import Data.List
import Data.Tree
import Data.Tuple



-- | The 'ears' function takes a graph with no node or edge annotation and
-- produces an ear decomposition. Each edge is annotated with a weight. Edges
-- with the same weight are in the same ear.

ears :: forall gr . DynGraph gr => gr () () -> gr () Int
ears g
  | isConnected g = gWs
  | otherwise = error "called ears on disconnected graph"
  where
    t :: Tree Node
    [t] = dff' g
    tps = treeToPaths t
    te = treeToEdges t
    g' = mkUGraph (nodes g) ((edges g \\ te) \\ (map swap te)) `asTypeOf` g
    -- gE is the graph of all edges not in the spanning tree. The edge weight
    -- is the distance between the tree root and the lowest common ancestor of
    -- the two nodes making up each edge
    gE :: gr () Int  = mkGraph (labNodes g') (map (lca tps) $ labEdges g')
    gE'  = mkGraph (labNodes gE) (labEdges gE ++ map (\(a,b) -> (a,b,0)) (te ++ map swap te))
    teWs = map (shortestPaths gE') te
    gWs  = mkGraph (labNodes g') (labEdges gE ++ teWs ++ map swap12 teWs)

shortestPaths :: Gr () Int -> Edge -> LEdge Int
shortestPaths g (u,v) = (u,v,spLength u v g') where
  g' = delEdge (u,v) $ delEdge (v,u) g

lca :: [[Node]] -> (LEdge ()) -> (LEdge Int)
lca ps (u,v,())
  | any null ps = error $ "null path: " ++ show ps
  | otherwise = (u,v,) . (subtract 1) . length . filter (uncurry (==)) $ zip u' v' where
  [u'] = filter ((==u) . last) ps
  [v'] = filter ((==v) . last) ps

swap12 (a,b,c) = (b,a,c)

sel3 (_,_,s) = s

-- tree edges

treeToEdges :: Tree Node -> [Edge]
treeToEdges (Node _ []) = []
treeToEdges (Node k xs) = map ((k,) . rootLabel) xs ++ concatMap treeToEdges xs

-- paths
treeToPaths :: Tree Node -> [[Node]]
treeToPaths (Node k []) = [[k]]
treeToPaths (Node k xs) = [[k]] ++ [ (k:ys) | ys <- concatMap treeToPaths xs ]



{-
 - Test graph
 -

fig13 :: Gr () ()
fig13 = undir $ mkUGraph ns es where
  ns = [1..9]
  es = [ (1,2),(1,4)
       , (2,3),(2,5)
       , (3,4),(3,6)
       , (4,7)
       , (5,6),(5,8)
       , (6,7)
       , (7,9)
       , (8,9)
       ]

-}

