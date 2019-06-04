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

module Example where

import Data.Aeson
import Data.Maybe
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Dependent.Sum (EqTag (..))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH
import Data.Text (Text)
import qualified Data.Text as T

import Data.Acid
import Data.Acid.Advanced

import Data.PropertyGraph
import Data.PropertyGraph.Acid


data VP a where
  VP_Title :: VP Text
  VP_Desc :: VP Text -- In Markdown
  VP_Ctx :: VP ()
  -- | Whether this context, and its tasks, are hidden by default
  VP_Hidden :: VP Bool


data EP a where
  -- | Whether this edge indicates the sorting sequence
  EP_Sequence :: EP ()
  -- | Whether this edge indicates a vertex dependency
  EP_Dependency :: EP ()

type Id = Integer

type ExampleGraph = PropertyGraph VP EP Id

type ExampleGraphEdit r = PropertyGraphEdit Id VP EP r

isHidden :: Ord a => a -> PropertyGraph VP ep a -> Bool
isHidden v g = fromMaybe False $ getVertexProperty VP_Hidden v g

isCtx :: Ord a => a -> PropertyGraph VP ep a -> Bool
isCtx v = maybe False (const True) . getVertexProperty VP_Ctx v

getTitle :: (Ord a, Show a) => a -> PropertyGraph VP ep a -> Text
getTitle v = fromMaybe ("Untitled:" <> T.pack (show v)) . getVertexProperty VP_Title v

getDesc :: Ord a => a -> PropertyGraph VP ep a -> Maybe Text
getDesc v = getVertexProperty VP_Desc v

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

-- FIXME:
instance IsAcidic ExampleGraph where
  acidEvents =
    [ UpdateEvent runSomeLabelledGraphEdit jsonMethodSerialiser
    , QueryEvent runSomeLabelledGraphView jsonMethodSerialiser
    ]

main :: IO ()
main = do
  putStrLn "okay"
