{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Types where

import Control.Exception (finally)
import Control.Monad (forM_, void)
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (EqTag (..), (==>))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Acid
import Data.Acid.Advanced

import Data.PropertyGraph
import Data.PropertyGraph.Acid
import Data.PropertyGraph.Labelled

-- | Vertex property types
data VP a where
  VP_Title :: VP Text
  VP_Desc :: VP Text
  VP_Ctx :: VP ()
  VP_Hidden :: VP Bool

-- | Edge property types
data EP a where
  -- | Whether this edge indicates the sorting sequence
  EP_Sequence :: EP ()
  -- | Whether this edge indicates a vertex dependency
  EP_Dependency :: EP ()

type ExampleGraph = PropertyGraph VP EP Integer

type ExampleGraphEdit r = PropertyGraphEdit Integer VP EP r

isHidden :: Ord a => a -> PropertyGraph VP ep a -> Bool
isHidden v g = fromMaybe False $ getVertexProperty VP_Hidden v g

isCtx :: Ord a => a -> PropertyGraph VP ep a -> Bool
isCtx v = isJust . getVertexProperty VP_Ctx v

getTitle :: (Ord a, Show a) => a -> PropertyGraph VP ep a -> Text
getTitle v = fromMaybe ("Untitled:" <> T.pack (show v)) . getVertexProperty VP_Title v

getDesc :: Ord a => a -> PropertyGraph VP ep a -> Maybe Text
getDesc = getVertexProperty VP_Desc

deriving instance (ToJSON v, ToJSONKey v) => ToJSON (PropertyGraph VP EP v)
deriving instance (FromJSON v, FromJSONKey v, Ord v) => FromJSON (PropertyGraph VP EP v)

instance EqTag EP Identity where
  eqTagged EP_Sequence EP_Sequence = (==)
  eqTagged EP_Dependency EP_Dependency = (==)
  eqTagged _ _ = (\_ _ -> False)

deriveGEq ''VP
deriveGCompare ''VP
deriveArgDict ''VP
deriveJSONGADT ''VP

deriveGEq ''EP
deriveGCompare ''EP
deriveArgDict ''EP
deriveJSONGADT ''EP

instance IsAcidic ExampleGraph where
  acidEvents =
    [ UpdateEvent runSomeLabelledGraphEdit jsonMethodSerialiser
    , QueryEvent runSomeLabelledGraphView jsonMethodSerialiser
    ]

main :: IO ()
main = do
  st <- openLocalState (LAM.empty :: ExampleGraph)
  f st `finally` closeAcidState st
  where
    f st = do
      -- Clear the graph
      void $ runEdit st PropertyGraphEdit_ClearAll
      -- Add our first vertex with given properties
      v1 <- runEdit st $ PropertyGraphEdit_AddVertex $ DMap.fromList [VP_Title ==> "First vertex title"]
      -- Add another
      v2 <- runEdit st $ PropertyGraphEdit_AddVertex $ DMap.fromList [VP_Title ==> "Second vertext"]
      -- Connect the two
      void $ runEdit st $ PropertyGraphEdit_AddEdge v1 v2 $ DMap.fromList [EP_Dependency ==> ()]
      -- Print the title of all vertices in the graph
      g <- runView st PropertyGraphView_All
      let vs :: [Integer] = unLabel <$> LAM.vertexList g
      forM_ vs $ \v -> print =<< runView st (PropertyGraphView_GetVertexProperty v VP_Title)
