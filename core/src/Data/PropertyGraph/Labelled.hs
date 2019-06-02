{-# LANGUAGE DeriveGeneric #-}

module Data.PropertyGraph.Labelled where

import Data.Aeson
import GHC.Generics
import Data.Function (on)

-- | A labelled type that ignores the label value in Eq and Ord instances
newtype Labelled a label = Labelled { unLabelled :: (a, label) }
  deriving Generic

emptyLabel :: Monoid label => a -> Labelled a label
emptyLabel x = Labelled (x, mempty)

labelled :: a -> label -> Labelled a label
labelled x l = Labelled (x, l)

unLabel :: Labelled a label -> a
unLabel = fst . unLabelled

getLabel :: Labelled a label -> label
getLabel = snd . unLabelled

instance Eq a => Eq (Labelled a label) where
  (==) = on (==) unLabel

instance Ord a => Ord (Labelled a label) where
  compare = on compare unLabel

instance (ToJSON a, ToJSON label) => ToJSON (Labelled a label)
instance (ToJSON a, ToJSON label) => ToJSONKey (Labelled a label)
instance (FromJSON a, FromJSON label) => FromJSON (Labelled a label)
instance (FromJSON a, FromJSON label) => FromJSONKey (Labelled a label)
