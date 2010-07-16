{-# LANGUAGE TypeFamilies #-}

module Data.RRT (Config (..), search, samples, controlEdgeProp) where

import Data.List (minimumBy)
import Data.List.Split (splitEvery)
import Data.Ord (comparing)
import Control.Monad.State
import System.Random

import Data.SpatialIndex

data Config state control index real edge = Config
    { -- | Propagation function
      propagation :: state -> control -> (edge, state)
      -- | Spatial index containing the initial state
    , spacialIndex :: index
      -- | Metric for measuring which control results in a state
      -- closest to a sample state
    , controlMetric :: state -> state -> real
    }

controlEdgeProp :: (s -> c -> s) -> (s -> c -> (c, s))
controlEdgeProp = (ap (,) .)

-- | Create a Rapidly exploring Random Tree (RRT) given a propagation
-- function, an initial state, and some randomly sampled states and
-- controls. The tree is returned as a list of (parent, edge, child)
-- triples which can be used as-is or placed into a proper tree
-- structure.
search :: (Real r, SpatialIndex index, Element index ~ state)
       => Config state control index r edge -- ^ Configuration parameters
       -> [(state, [control])]        -- ^ Random states and controls
       -> [(edge, state)]             -- ^ Resulting tree as a list of nodes
search config stream =
    fst $ runState (mapM (explore config) stream) (spacialIndex config)


-- | Make a single step in the RRT algorithm updating intermediate state.
explore :: (Real r, SpatialIndex index, Element index ~ state)
        => Config state control index r edge -- ^ Configuration parameters
        -> (state, [control])                -- ^ Random sample
        -> State index (edge, state)         -- ^ New tree node
explore config sample =
    do index <- get
       let edge@(_, child) = expand config index sample
       let index' = insert child index
       put index'
       return edge


-- | Given a spatial index of all the states in a tree, return a new edge.
expand :: (Real r, SpatialIndex index, Element index ~ state)
       => Config state control index r edge -- ^ Configuration parameters
       -> index                       -- ^ For performing nearest neighbor query
       -> (state, [control])          -- ^ State to try to expand to using one
                                      -- of the possible controls
       -> (edge, state)               -- ^ New tree edge with new child node
expand config index (sample, controls) = node
    where
      parent = index `nearest` sample
      node = choose (propagation config parent)
                    (controlMetric config sample)
                    controls

-- | Return the closest state to the sample along with the control
-- that causes it to propagate there.
choose :: Real r
       => (control -> (edge, state)) -- ^ Propagation function
       -> (state -> r)               -- ^ Distance to desired state
       -> [control]                  -- ^ Possible controls
       -> (edge, state)              -- ^ Edge and the resulting state
choose prop distance controls = snd $ minimumBy (comparing fst) results
    where
      -- The (edge, state)s achieved after applying all controls
      (edges, states) = unzip $ map prop controls
      -- The distance of each of the new states to the sample
      distances = map distance states
      -- Bundle everything together so the closest (control,state) can
      -- be found
      results = zip distances (zip edges states)


-- | Generate a stream of random states and lists of controls within ranges.
samples :: (Random state, Random control, RandomGen g)
           => Int                  -- ^ Number of controls per sample
           -> (state, state)       -- ^ Range to sample states from
           -> (control, control)   -- ^ Range to sample controls from
           -> g                    -- ^ Initial state of random number generator
           -> [(state, [control])] -- ^ An infinite stream of samples
samples n sr cr g = zip states controls
    where
      (g1, g2) = split g
      states = randomRs sr g1
      controls = splitEvery n $ randomRs cr g2
