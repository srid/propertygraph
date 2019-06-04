{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Data storage types
module Data.PropertyGraph where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint
import Data.Constraint.Extras
import Data.Dependent.Map (DMap)
import Data.Dependent.Map
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM

import Data.PropertyGraph.Internal ()
import Data.PropertyGraph.Labelled


-- TODO: Improvements to do before publishing on hackage
-- - Wrap library functions so users dont have to use `emptyLabel` or `unLabel`
-- - Test ghcjs build to know what to put in -core vs regular.

-- | A graph where both edges and vertices have associated labels
type LabelledGraph v e a = LAM.AdjacencyMap e (Labelled a v)

getVertexLabel :: (Ord a, Monoid v) => a -> LabelledGraph v e a -> v
getVertexLabel x am = maybe mempty getLabel $ lookupKey (emptyLabel x) $ LAM.adjacencyMap am
  where
    lookupKey :: Ord k => k -> Map k a -> Maybe k
    lookupKey k m = fst . flip Map.elemAt m <$> Map.lookupIndex k m

setVertexLabel :: (Ord a, Eq e, Monoid e, Monoid v) => a -> v -> LabelledGraph v e a -> LabelledGraph v e a
setVertexLabel vertex x = LAM.replaceVertex (emptyLabel vertex) (labelled vertex x)

-- | A labelled graph where label is a set of properties
type PropertyGraph vp ep a = LabelledGraph (DMap vp Identity) (DMap ep Identity) a

getVertexProperty :: (GCompare vp, Ord a) => vp c -> a -> PropertyGraph vp ep a -> Maybe c
getVertexProperty p x g = getProperty p $ getVertexLabel x g

getEdgeProperty
  :: (GCompare ep, GCompare vp, Ord a)
  => ep c -> a -> a -> PropertyGraph vp ep a -> Maybe c
getEdgeProperty p x y g = getProperty p label
  where
    label = LAM.edgeLabel (emptyLabel x) (emptyLabel y) g

getProperty :: GCompare f => f a -> DMap f Identity -> Maybe a
getProperty p m = fmap runIdentity $ DMap.lookup p m

-- | Edit operations for `LabelledGraph`
data LabelledGraphEdit v vm em :: * -> * where
  LabelledGraphEdit_ClearAll :: LabelledGraphEdit v vm em ()
  LabelledGraphEdit_AddVertex :: vm -> LabelledGraphEdit v vm em v
  LabelledGraphEdit_AddEdge :: v -> v -> em -> LabelledGraphEdit v vm em ()
  LabelledGraphEdit_SetVertexProperties :: v -> vm -> LabelledGraphEdit v vm em ()
  LabelledGraphEdit_SetEdgeProperties :: v -> v -> em -> LabelledGraphEdit v vm em ()

-- | PropertyGraphEdit operations for `PropertyGraph`
data PropertyGraphEdit v vp ep r where
  PropertyGraphEdit_ClearAll :: PropertyGraphEdit v vp ep ()
  PropertyGraphEdit_AddVertex :: DMap vp Identity -> PropertyGraphEdit v vp ep v
  PropertyGraphEdit_AddEdge :: v -> v -> DMap ep Identity -> PropertyGraphEdit v vp ep ()
  PropertyGraphEdit_SetVertexProperty :: GCompare vp => v -> DSum vp Identity -> PropertyGraphEdit v vp ep ()
  PropertyGraphEdit_SetEdgeProperty :: GCompare ep => v -> v -> DSum ep Identity -> PropertyGraphEdit v vp ep ()

-- | View operations for `LabelledGraph`
data LabelledGraphView v vm em :: * -> * where
  LabelledGraphView_Unused :: v -> vm -> em -> LabelledGraphView v vm em () -- Don't use this. https://github.com/obsidiansystems/aeson-gadt-th/issues/14
  LabelledGraphView_All :: LabelledGraphView v vm em (LabelledGraph vm em v)
  LabelledGraphView_GetVertexProperties :: v -> LabelledGraphView v vm em vm
  LabelledGraphView_GetEdgeProperties :: v -> v -> LabelledGraphView v vm em em

-- | View operations for `PropertyGraph`
data PropertyGraphView v vp ep r where
  PropertyGraphView_All :: PropertyGraphView v vp ep (PropertyGraph vp ep v)
  PropertyGraphView_GetVertexProperty :: GCompare vp => v -> vp a -> PropertyGraphView v vp ep (Maybe a)
  PropertyGraphView_GetEdgeProperty :: GCompare ep => v -> v -> ep a -> PropertyGraphView v vp ep (Maybe a)

-- Derive manually because `deriveArgDict` is broken for type variables in constructor arguments
instance ArgDict (LabelledGraphEdit v vm em) where
  type ConstraintsFor (LabelledGraphEdit v vm em) c = (c v, c (), c vm, c em)
  argDict = \case
    LabelledGraphEdit_ClearAll {} -> Dict
    LabelledGraphEdit_AddVertex {} -> Dict
    LabelledGraphEdit_AddEdge {} -> Dict
    LabelledGraphEdit_SetVertexProperties {} -> Dict
    LabelledGraphEdit_SetEdgeProperties {} -> Dict

instance ArgDict (LabelledGraphView v vm em) where
  type ConstraintsFor (LabelledGraphView v vm em) c = (c (), c vm, c em, c (LabelledGraph vm em v))
  argDict = \case
    LabelledGraphView_Unused _ _ _ -> Dict
    LabelledGraphView_All -> Dict
    LabelledGraphView_GetVertexProperties _ -> Dict
    LabelledGraphView_GetEdgeProperties _ _ -> Dict

deriveJSONGADT ''LabelledGraphEdit
deriveJSONGADT ''LabelledGraphView
-- deriveJSONGADT ''PropertyGraphEdit
-- deriveJSONGADT ''PropertyGraphView

