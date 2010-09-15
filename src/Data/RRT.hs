{-# LANGUAGE TypeFamilies #-}

module Data.RRT (Config (..), search, samples, controlEdgeProp) where

import Data.List (minimumBy)
import Data.List.Split (splitEvery)
import Data.Maybe
import Data.Ord (comparing)
import Control.Monad.State
import System.Random

import Data.SpatialIndex

data (Real real, SpatialIndex index, Element index ~ state)
    => Config state control index real edge = Config
    { -- | Propagation function
      propagation :: state -> control -> (edge, state)
      -- | Spatial index containing the initial state
    , spacialIndex :: index
      -- | Metric for measuring which control results in a state
      -- closest to a sample state
    , controlMetric :: state -> state -> real
      -- | Collision detection function. Paths with collisions will be rejected.
    , collided :: state -> Bool
    }

controlEdgeProp :: (s -> c -> s) -> (s -> c -> (c, s))
controlEdgeProp = (ap (,) .)

-- | Create a Rapidly exploring Random Tree (RRT) given a propagation
-- function, an initial state, and some randomly sampled states and
-- controls. The tree is returned as a list of (parent, edge, child)
-- triples which can be used as-is or placed into a proper tree
-- structure.
search :: (Real r, SpatialIndex index, Element index ~ state, Eq state)
       => Config state control index r edge -- ^ Configuration parameters
       -> [(state, [control])]        -- ^ Random states and controls
       -> [(edge, state)]             -- ^ Resulting tree as a list of nodes
search config stream = catMaybes $ fst 
                     $ runState (mapM (explore config) stream) 
                     $ spacialIndex config


-- | Make a single step in the RRT algorithm updating intermediate state.
explore :: (Real r, SpatialIndex index, Element index ~ state, Eq state)
        => Config state control index r edge -- ^ Configuration parameters
        -> (state, [control])                -- ^ Random sample
        -> State index (Maybe (edge, state)) -- ^ New tree node
explore config sample =
    do index <- get
       let maybeEdge = expand config index sample
       if isJust maybeEdge 
         then put $ insert (snd $ fromJust maybeEdge) index
         else return ()
       return maybeEdge


-- | Given a spatial index of all the states in a tree, return a new edge.
expand :: (Real r, SpatialIndex index, Element index ~ state, Eq state)
       => Config state control index r edge -- ^ Configuration parameters
       -> index                       -- ^ For performing nearest neighbor query
       -> (state, [control])          -- ^ State to try to expand to using one
                                      -- of the possible controls
       -> Maybe (edge, state)         -- ^ New tree edge with new child node
expand config index (sample, controls) = if selfEdge then Nothing else node
    where
      parent = index `nearest` sample
      node = genChoices (propagation config parent)
                        (controlMetric config sample)
                        (not . collided config)
                        controls
      selfEdge = if isJust node
                   then (snd $ fromJust node) == parent -- Check for self edge
                   else False
                         -- Self-edge means prop couldn't advance the state
                         -- This is such a mess. Do it in the Maybe monad.

-- | 
genChoices :: Real r
           => (control -> (edge, state)) -- ^ Propagation function
           -> (state -> r)               -- ^ Distance to desired state
           -> (state -> Bool)            -- ^ True if not colliding
           -> [control]                  -- ^ Possible controls
           -> Maybe (edge, state)        -- ^ Edge and the resulting state
genChoices prop distance notColliding controls = choose choices
    where
      -- The (edge, state)s achieved after applying all controls
      (edges, states) = unzip $ filter (notColliding . snd) $ map prop controls
      -- The distance of each of the new states to the sample
      distances = map distance states
      -- Bundle everything together so the closest (control,state) can
      -- be found
      choices = zip distances (zip edges states)

-- | Return the closest state to the sample along with the control
-- that causes it to propagate there.
choose :: Real r => [(r, (edge, state))] -> Maybe (edge, state)
choose [] = Nothing
choose choices = Just $ snd $ minimumBy (comparing fst) choices

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
