{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.PropertyGraph.Internal where

import Data.Aeson
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Some (Some)
import qualified Data.Some as Some

-- TODO: upstream to obsidian
instance (ForallF ToJSON f) => ToJSON (Some f) where
  toJSON = \case
    Some.This (x :: f a) -> whichever @ToJSON @f @a (toJSON x)
