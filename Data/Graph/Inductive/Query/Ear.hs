{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Ear decomposition of a graph.

module Data.Graph.Inductive.Query.Ear where

import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Example
import Data.Tree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Tree
import Data.List
import Data.Function
import Data.Tuple



-- test = ears $ undir dag4
test = let g = (undir $ dag4 ) -- ucycle 6 :: Gr () ())
       in do print g
             print ""
             print $ ears g

-- | Perform the actual decomposition
--
-- TODO fix type!

-- ears :: DynGraph gr => gr a b -> [gr a b]
ears g = (groupBy ((==) `on` sel3) $ sortBy (compare `on` sel3) $ filter (uncurry (<) . sel12) $ remEdges ++ treEdges) where
  mst :: Tree Node
  [mst] = dff' g
  tes   = mkEdges mst
  treeg :: Gr () () = mkUGraph (nub $ map fst tes ++ map snd tes) tes
  grt   = delEdges (tes ++ map swap tes) g
  uvs   = edges grt
  ps    = mkPaths mst
  g' :: Gr () Int = mkGraph (map (,()) $ nodes g) [] -- (remEdges ++ treEdges)
  remEdges = sort [(x,y,length $ path ps x y) | (x,y) <- uvs]
  treEdges = sort [(x,y, lambdaT x y) | (x,y) <- tes]
  lambdaT x y = minimum $ 99999 : [ l | (u,v,l) <- remEdges
                                  , let u' = nodePath ps u
                                  , let v' = nodePath ps v
                                  , let uvp = toPath u' \\ toPath v'
                                  , let vup = toPath v' \\ toPath u'
                                  , (x,y) `elem` uvp || (x,y) `elem` vup || (y,x) `elem` uvp || (y,x) `elem` vup
                                  ]
  es = map (map sel12)
     . groupBy ((==) `on` sel3)
     . filter ((<99999) . sel3)
     . sortBy (compare `on` sel3)
     $ labEdges g'
  zs = (remEdges,treEdges)
  dbg = (1,2, [ (u,v,l,u',v')
              | (u,v,l) <- remEdges
              , let u' = nodePath ps u
              , let v' = nodePath ps v
              ]
          )

swap12 (a,b,c) = (b,a,c)
sel12 (a,b,_) = (a,b)
sel3 (_,_,s) = s

inPath (x,y) = f where
  f []  = False
  f [u] = False
  f (u:v:ws) = x==u && y==v || f (v:ws)

toPath :: [Node] -> [Edge]
toPath [] = []
toPath [x] = []
toPath (x:y:zs) = (x,y) : toPath (y:zs)

-- tree path for a node

nodePath :: [[Node]] -> Node -> [Node]
nodePath ps n = head $ filter ((==n) . last) ps

-- unique path

uniquePathFst :: [[Node]] -> Node -> Node -> [Node]
uniquePathFst ps a b = map fst $ dropWhile (uncurry (==)) $ zip as bs where
  [as] = filter ((==a) . last) $ ps
  [bs] = filter ((==b) . last) $ ps

-- common path

path :: [[Node]] -> Node -> Node -> [Node]
path ps a b = map fst $ takeWhile (uncurry (==)) $ zip as bs where
  [as] = filter ((==a) . last) $ ps
  [bs] = filter ((==b) . last) $ ps

-- tree edges

mkEdges :: Tree Node -> [Edge]
mkEdges (Node _ []) = []
mkEdges (Node k xs) = map ((k,) . rootLabel) xs ++ concatMap mkEdges xs

-- paths

mkPaths :: Tree Node -> [[Node]]
mkPaths (Node k []) = [[k]]
mkPaths (Node k xs) = [[k]] ++ [ (k:ys) | ys <- concatMap mkPaths xs ]
