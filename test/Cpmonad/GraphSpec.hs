{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Cpmonad.GraphSpec where

import Control.Monad (forM_, replicateM, when)
import Cpmonad.Gen hiding (choose, shuffle)
import Cpmonad.Gen qualified
import Cpmonad.TestUtil (covers)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import Data.Vector qualified as V

-- import System.Random (mkStdGen)

import Control.Arrow ((&&&))
import Control.Monad.ST
import Cpmonad.Graph
import Data.Graph.Haggle
import Data.Graph.Haggle.Algorithms.DFS (components, isConnected)
import Data.Graph.Haggle.Classes (maxVertexId)
import Data.Maybe
import Data.MultiSet qualified as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Gen)
import Prelude hiding (print)

checkGraphProperty :: GraphOptions -> Digraph -> _
checkGraphProperty GraphOptions{..} g = do
  counterexample (show $ vertices g) $
    counterexample (show $ edges g) $
      conjoin
        ( [ counterexample "connected" $
              not gIsConnected ==> connected === False
          , counterexample "acyclic" $
              not gHasCycle ==> acyclic === False
          , counterexample "directed" $
              gDirected ==> directed === True
          , counterexample "allowLoops" $
              gHasLoops ==> allowLoops === True
          , counterexample "allowMulti" $
              gHasMulti ==> allowMulti === True
          , counterexample "allowAntiparallel" $
              gHasAntiparallel ==> allowAntiparallel === True
          ]
        )
 where
  n = maxVertexId g
  h = runST do
    h <- newSizedMBiDigraph n 0
    forM_ (edges g) \(edgeSource &&& edgeDest -> (a, b)) -> addEdge h a b
    freeze h
  gEdges = Set.fromList $ (edgeSource &&& edgeDest) <$> edges g
  gRevEdges = Set.map (\(a, b) -> (b, a)) gEdges
  gIsConnected = isConnected h
  gDirected = not . Set.null $ Set.difference gEdges gRevEdges
  gHasLoops = any (uncurry (==)) gEdges
  gHasMulti = (length . nubOrd $ Set.toList gEdges) == Set.size gEdges
  gHasAntiparallel = (length . nubOrd $ Set.toList gEdges ++ Set.toList gRevEdges) == Set.size gEdges + Set.size gRevEdges
  gHasCycle = isJust $ findCycle g

spec :: Spec
spec = describe "gengraph" do
  let o = disconnected
  prop "works" $
    \(getPositive -> n)
     (getPositive -> m) ->
        (m <= maxEdges o n)
          ==> \(runGen (gengraph o n m) -> (g, _)) ->
            checkGraphProperty o g
