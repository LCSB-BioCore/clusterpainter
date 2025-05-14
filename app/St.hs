{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module St where

import Data.Aeson
import qualified Data.Set as S
import qualified Data.Vector as V
import DearImGui.Internal.Text
import Lens.Micro
import Lens.Micro.Internal (Index, IxValue, Ixed)
import Lens.Micro.TH

type Ve = V.Vector

data AppState = St
  { _clusters :: Ve Cluster
  , _featureNames :: Ve Text
  , _groupNames :: Ve Text
  , _syncOutFile :: Maybe FilePath
  , _hiFeature :: Int
  } deriving (Show)

data Cluster = Cluster
  { _position :: (Float, Float)
  , _weight :: Float
  , _features :: Ve Float
  , _featMeans :: Ve Float
  , _featVars :: Ve Float
  , _selected :: Bool
  , _groups :: S.Set Int
  } deriving (Show)

makeLenses ''AppState

makeLenses ''Cluster

data FileState = FSt
  { fsClusterGroups :: [[Bool]]
  , fsFeatures :: [String]
  , fsGroups :: [String]
  } deriving (Show)

instance FromJSON FileState where
  parseJSON =
    withObject "State" $ \v ->
      FSt <$> v .: "groups" <*> v .: "feature_names" <*> v .: "group_names"

instance ToJSON FileState where
  toJSON (FSt ig fs gs) =
    object ["groups" .= ig, "feature_names" .= fs, "group_names" .= gs]
