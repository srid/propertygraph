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

module Data.PropertyGraph.Acid where

import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, modify)
import Data.Aeson
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Typeable
import Safe.Foldable (maximumMay)

import qualified Data.Some as Some
import Data.Dependent.Map (DMap, DSum ((:=>)), Some)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum.Orphans ()

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.SafeCopy
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Core

import Data.PropertyGraph.Labelled
import Data.PropertyGraph

-- TODO: these should go to example
-- import Common.Types

runSomeLabelledGraphView
  :: (Ord v, Monoid vm, Monoid em)
  => Some (LabelledGraphView v vm em) -> Query (LabelledGraph vm em v) (DSum (LabelledGraphView v vm em) Identity)
runSomeLabelledGraphView = \case
  Some.This v@(LabelledGraphView_Unused _ _ _) -> pure $ v :=> Identity ()
  Some.This LabelledGraphView_All -> do
    g :: LabelledGraph vm em v <- ask
    pure $ LabelledGraphView_All :=> Identity g
  Some.This (LabelledGraphView_GetVertexProperties v) -> do
    g <- ask
    pure $ LabelledGraphView_GetVertexProperties v :=> Identity (getVertexLabel v g)
  Some.This (LabelledGraphView_GetEdgeProperties v1 v2) -> do
    g <- ask
    let r = LAM.edgeLabel (emptyLabel v1) (emptyLabel v2) g
    pure $ LabelledGraphView_GetEdgeProperties v1 v2 :=> Identity r

runSomeLabelledGraphEdit
  :: (Ord v, Num v, Monoid vp, Monoid ep, Eq ep)
  => Some (LabelledGraphEdit v vp ep) -> Update (LabelledGraph vp ep v) (DSum (LabelledGraphEdit v vp ep) Identity)
runSomeLabelledGraphEdit = \case
  Some.This LabelledGraphEdit_ClearAll -> do
    modify $ \_ -> LAM.empty
    pure $ LabelledGraphEdit_ClearAll :=> Identity ()
  Some.This (LabelledGraphEdit_AddVertex meta) -> do
    modify $ \g ->
      let nv = 1 + getMaximumId g
      in LAM.overlay g $ LAM.vertex (labelled nv meta)
    nv <- getMaximumId <$> get
    pure $ LabelledGraphEdit_AddVertex meta :=> Identity nv
  Some.This (LabelledGraphEdit_AddEdge a b meta) -> do
    modify $ \g -> LAM.overlay g $ LAM.edge meta (emptyLabel a) (emptyLabel b)
    pure $ LabelledGraphEdit_AddEdge a b meta :=> Identity ()
  Some.This (LabelledGraphEdit_SetVertexProperties v p) -> do
    modify $ \g -> setVertexLabel v p g
    pure $ LabelledGraphEdit_SetVertexProperties v p :=> Identity ()
  Some.This (LabelledGraphEdit_SetEdgeProperties v1 v2 p) -> do
    modify $ \g -> LAM.replaceEdge p (emptyLabel v1) (emptyLabel v2) g
    pure $ LabelledGraphEdit_SetEdgeProperties v1 v2 p :=> Identity ()
  where
    getMaximumId g = fromMaybe 0 $ maximumMay $ Set.map unLabel $ Map.keysSet $ LAM.adjacencyMap g

runLabelledGraphView
  :: forall v vp ep r. (Typeable v, Typeable vp, Typeable ep)
  => AcidState (LabelledGraph vp ep v) -> LabelledGraphView v vp ep r -> IO r
runLabelledGraphView st e = getViewResult e <$> query st (Some.This e)

runView
  :: forall v vp ep r. (Typeable v, Typeable (DMap vp Identity), Typeable (DMap ep Identity))
  => AcidState (PropertyGraph vp ep v) -> PropertyGraphView v vp ep r -> IO r
runView st pe = case pe of
  PropertyGraphView_All -> runLabelledGraphView st $ LabelledGraphView_All
  PropertyGraphView_GetVertexProperty v p -> do
    vprops :: DMap vp Identity <- runLabelledGraphView st $ LabelledGraphView_GetVertexProperties v
    pure $ fmap runIdentity $ DMap.lookup p vprops
  PropertyGraphView_GetEdgeProperty v1 v2 p -> do
    eprops :: DMap ep Identity <- runLabelledGraphView st $ LabelledGraphView_GetEdgeProperties v1 v2
    pure $ fmap runIdentity $ DMap.lookup p eprops

runLabelledGraphEdit
  :: forall v vp ep r. (Typeable v, Typeable vp, Typeable ep)
  => AcidState (LabelledGraph vp ep v) -> LabelledGraphEdit v vp ep r -> IO r
runLabelledGraphEdit st e = getEditResult e <$> update st (Some.This e)

-- TODO: get rid of the get*Result functions; case unwrapping shoud be close to Some.This code
getViewResult :: LabelledGraphView v vp ep r -> DSum (LabelledGraphView v vp ep) Identity -> r
getViewResult e se = case e of
  LabelledGraphView_Unused _ _ _ -> let LabelledGraphView_Unused _ _ _ :=> Identity r = se in r
  LabelledGraphView_All -> let LabelledGraphView_All :=> Identity r = se in r
  LabelledGraphView_GetVertexProperties _ -> let LabelledGraphView_GetVertexProperties _ :=> Identity r = se in r
  LabelledGraphView_GetEdgeProperties _ _ -> let LabelledGraphView_GetEdgeProperties _ _ :=> Identity r = se in r

getEditResult :: LabelledGraphEdit v vp ep r -> DSum (LabelledGraphEdit v vp ep) Identity -> r
getEditResult e se = case e of
  LabelledGraphEdit_ClearAll {} -> let LabelledGraphEdit_ClearAll {} :=> Identity r = se in r
  LabelledGraphEdit_AddVertex {} -> let LabelledGraphEdit_AddVertex {} :=> Identity r = se in r
  LabelledGraphEdit_AddEdge {} -> let LabelledGraphEdit_AddEdge {} :=> Identity r = se in r
  LabelledGraphEdit_SetVertexProperties {} -> let LabelledGraphEdit_SetVertexProperties {}  :=> Identity r = se in r
  LabelledGraphEdit_SetEdgeProperties {} -> let LabelledGraphEdit_SetEdgeProperties {} :=> Identity r = se in r

runEdit
  :: forall v vp ep r. (Typeable v, Typeable (DMap vp Identity), Typeable (DMap ep Identity))
  => AcidState (PropertyGraph vp ep v) -> PropertyGraphEdit v vp ep r -> IO r
runEdit st pe = case pe of
  PropertyGraphEdit_ClearAll -> runLabelledGraphEdit st LabelledGraphEdit_ClearAll
  PropertyGraphEdit_AddVertex vm -> runLabelledGraphEdit st $ LabelledGraphEdit_AddVertex vm
  PropertyGraphEdit_AddEdge v1 v2 meta -> runLabelledGraphEdit st $ LabelledGraphEdit_AddEdge v1 v2 meta
  PropertyGraphEdit_SetVertexProperty v (p :=> Identity val) -> do
    vprops :: DMap vp Identity <- runLabelledGraphView st $ LabelledGraphView_GetVertexProperties v
    runLabelledGraphEdit st $ LabelledGraphEdit_SetVertexProperties v $ DMap.insert p (Identity val) vprops
  PropertyGraphEdit_SetEdgeProperty v1 v2 (p :=> Identity val) -> do
    eprops :: DMap ep Identity <- runLabelledGraphView st $ LabelledGraphView_GetEdgeProperties v1 v2
    runLabelledGraphEdit st $ LabelledGraphEdit_SetEdgeProperties v1 v2 $ DMap.insert p (Identity val) eprops

instance (Typeable r, Typeable v, Typeable vp, Typeable ep) => Method (LabelledGraphEdit v vp ep r) where
  type MethodResult (LabelledGraphEdit v vp ep r) = r
  type MethodState (LabelledGraphEdit v vp ep r) = LabelledGraph vp ep v

instance (Typeable v, Typeable vp, Typeable ep) => UpdateEvent (Some (LabelledGraphEdit v vp ep))
instance (Typeable v, Typeable vp, Typeable ep) => Method (Some (LabelledGraphEdit v vp ep)) where
  type MethodResult (Some (LabelledGraphEdit v vp ep)) = DSum (LabelledGraphEdit v vp ep) Identity
  type MethodState (Some (LabelledGraphEdit v vp ep)) = LabelledGraph vp ep v

instance (Typeable r, Typeable v, Typeable vp, Typeable ep) => Method (LabelledGraphView v vp ep r) where
  type MethodResult (LabelledGraphView v vp ep r) = r
  type MethodState (LabelledGraphView v vp ep r) = LabelledGraph vp ep v

instance (Typeable v, Typeable vp, Typeable ep) => QueryEvent (Some (LabelledGraphView v vp ep))
instance (Typeable v, Typeable vp, Typeable ep) => Method (Some (LabelledGraphView v vp ep)) where
  type MethodResult (Some (LabelledGraphView v vp ep)) = DSum (LabelledGraphView v vp ep) Identity
  type MethodState (Some (LabelledGraphView v vp ep)) = LabelledGraph vp ep v


jsonMethodSerialiser
  :: ( FromJSON method ,FromJSON (MethodResult method) ,ToJSON method ,ToJSON (MethodResult method))
  => MethodSerialiser method
jsonMethodSerialiser = MethodSerialiser jsonSerialiser jsonSerialiser
  where
    jsonSerialiser :: (FromJSON a, ToJSON a) => Serialiser a
    jsonSerialiser = Serialiser encode eitherDecode


instance SafeCopy (LAM.AdjacencyMap e a) where
  putCopy = contain . safePut
  getCopy = contain safeGet
