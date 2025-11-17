{-# LANGUAGE NoFieldSelectors #-}

{-
Copyright (c) 2025 drdilyor

Some parts of this code has been translated from
https://github.com/ifsmirnov/jngen with the following license:

MIT License

Copyright (c) 2016-2018 Ivan Smirnov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

module Cpmonad.Graph (
  GraphOptions (..),
  connected,
  disconnected,
  gengraph,
  gentree,
  gentreePrim,
  gentreeKruskal,
  treeFromPruferCode,
  bambooTree,
  lineTree,
  starTree,
  shuffleGraph,
  linkGraphs,
  glueGraphs,
  vi,
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Data.Function
import Data.Graph.Haggle
import Data.Graph.Haggle.Classes (maxVertexId)
import Data.STRef
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Unsafe.Coerce (unsafeCoerce)

import Cpmonad.Gen

data GraphOptions = GraphOptions
  { connected :: Bool
  , directed :: Bool
  , allowLoops :: Bool
  , allowMulti :: Bool
  , allowAntiparallel :: Bool
  , acyclic :: Bool
  }

disconnected :: GraphOptions
disconnected =
  GraphOptions
    { connected = False
    , directed = False
    , allowLoops = False
    , allowMulti = False
    , allowAntiparallel = False
    , acyclic = False
    }

connected :: GraphOptions
connected = disconnected{connected = False}

vi :: Int -> Vertex
vi = unsafeCoerce

gengraph :: GraphOptions -> Int -> Int -> Gen Digraph
gengraph GraphOptions{..} n m
  | not connected && not directed && acyclic = error "gengraph doesn't have ability to generate forests!"
  | connected && m < n - 1 = error "not enough edges for a connected graph"
  | not allowMulti && m > maxEdges = error "too many edges in the graph"
 where
  maxEdges =
    (n * (n - 1))
      & applyWhen (directed && allowAntiparallel) (`div` 2)
      & applyWhen allowLoops (+ n)
gengraph GraphOptions{..} n m = runGenST do
  resEdges <- lift $ newSTRef Set.empty
  when connected do
    tree <- liftg $ gentree n
    forM_ (edges tree) \(edgeSource &&& edgeDest -> (a, b)) -> do
      (a, b) <-
        if not directed
          then pure (a, b)
          else (\case 0 -> (a, b); _ -> (b, a)) <$> liftg (genr 0 2)
      lift $ modifySTRef' resEdges $ Set.insert (vertexId a, vertexId b)

  let
    go resEdges | Set.size resEdges == m = pure resEdges
    go resEdges = do
      edge <- liftg $ liftA2 (,) (genr 0 n) (genr 0 n)
      if
        | not allowLoops && fst edge == snd edge -> go resEdges
        | not allowMulti && Set.member edge resEdges -> go resEdges
        | directed && not allowAntiparallel && Set.member (snd edge, fst edge) resEdges -> go resEdges
        | otherwise -> go (Set.insert edge resEdges)

  resEdges' <- go =<< lift (readSTRef resEdges)
  g <- newSizedMDigraph n m
  replicateM_ n $ addVertex g
  forM_ (Set.toList resEdges') \(a, b) -> addEdge g (vi a) (vi b)
  freeze g

gentree :: Int -> Gen SimpleBiDigraph
gentree n = treeFromPruferCode <$> V.replicateM (n - 2) (genr 0 n)

treeFromPruferCode :: Vector Int -> SimpleBiDigraph
treeFromPruferCode code = runST do
  let n = V.length code + 2
  g <- newSizedMSimpleBiDigraph n (n - 1)
  degree <- VM.replicate n (1 :: Int)
  leaves <- newSTRef Set.empty

  V.forM_ code $ VM.modify degree (+ 1)
  replicateM_ n $ addVertex g

  forM_ [0 .. n - 1] \i -> do
    di <- VM.read degree i
    when (di == 1) $ modifySTRef' leaves (Set.insert (vi i))

  let
    go i leaves
      | i == n - 2 =
          void $ addEdge g (Set.findMin leaves) (Set.findMax leaves)
    go i (Set.minView -> Just (leaf, leaves)) = do
      let v = code V.! i
      void $ addEdge g leaf (vi v)
      VM.modify degree (subtract 1) v
      di <- VM.read degree v
      go (i + 1) $ if di == 1 then Set.insert (vi v) leaves else leaves
    go _ _ = error "infallible"
  -- collect vertices with degree == 1
  leaves <- VM.ifoldl' (\cases s i 1 -> Set.insert (vi i) s; s _ _ -> s) Set.empty degree
  go 0 leaves

  freeze g

gentreePrim :: Int -> Int -> Gen SimpleBiDigraph
gentreePrim n elongation = runGenST do
  g <- newSizedMSimpleBiDigraph n (n - 1)
  void $ addVertex g
  forM_ [1 .. n - 1] \i -> do
    v <- addVertex g
    parent <- liftg $ genrw 0 (i - 1) elongation
    addEdge g (vi parent) v
  freeze g

gentreeKruskal :: Int -> Gen SimpleBiDigraph
gentreeKruskal n = runGenST do
  pure undefined

lineTree :: Int -> SimpleBiDigraph
lineTree n = runST do
  g <- newSizedMSimpleBiDigraph n (n - 1)
  forM_ [0 .. n - 2] \i -> addEdge g (vi i) (vi $ i + 1)
  freeze g

bambooTree :: Int -> SimpleBiDigraph
bambooTree = lineTree

starTree :: Int -> SimpleBiDigraph
starTree n = runST do
  g <- newSizedMSimpleBiDigraph n (n - 1)
  root <- addVertex g
  replicateM_ n $ addVertex g >>= addEdge g root
  freeze g

gentreeCaterpillar :: Int -> Int -> Gen SimpleBiDigraph
gentreeCaterpillar = error "TODO"

binaryTree :: Int -> Int -> SimpleBiDigraph
binaryTree = error "TODO"

karyTree :: Int -> Int -> SimpleBiDigraph
karyTree = error "TODO"

-- TODO: generalize for more graph, generalize for labels
shuffleGraph :: SimpleBiDigraph -> Gen SimpleBiDigraph
shuffleGraph g = runGenST do
  h <- newSizedMSimpleBiDigraph (maxVertexId g) 0
  vertexList <- V.fromList <$> sequence (addVertex h <$ vertices g)
  vertexList <- liftg $ shuffle vertexList
  forM_ (edges g) \(edgeSource &&& edgeDest -> (a, b)) ->
    addEdge h (vertexList V.! vertexId a) (vertexList V.! vertexId b)
  freeze h

-- TODO: decide whether to use Int or Vertex
linkGraphs :: (Thawable g, MAddVertex (MutableGraph g), MAddEdge (MutableGraph g)) => Int -> g -> Int -> g -> ImmutableGraph (MutableGraph g)
linkGraphs v1 g1 v2 g2 = runST do
  g <- thaw g1
  n <- countVertices g
  replicateM_ (maxVertexId g2 + 1) $ addVertex g
  forM_ (edges g2) \(edgeSource &&& edgeDest -> (a, b)) ->
    addEdge g (vi $ n + vertexId a) (vi $ n + vertexId b)
  void $ addEdge g (vi v1) (vi $ n + v2)
  freeze g

glueGraphs :: (Thawable g, MAddVertex (MutableGraph g), MAddEdge (MutableGraph g)) => Int -> g -> Int -> g -> ImmutableGraph (MutableGraph g)
glueGraphs v1 g1 v2 g2 = runST do
  g <- thaw g1
  n <- countVertices g
  replicateM_ (maxVertexId g2 + 1 - 1) $ addVertex g
  let
    newVertex (vertexId -> x)
      | x == v2 = vi v1
      | x < v2 = vi $ n + x
      | otherwise = vi $ n + x - 1
  forM_ (edges g2) \(edgeSource &&& edgeDest -> (a, b)) ->
    addEdge g (newVertex a) (newVertex b)
  freeze g
