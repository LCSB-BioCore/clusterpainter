{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module St where

import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Aeson
import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import DearImGui.Internal.Text
import SDL.Vect

type Ve = V.Vector

data AppState = St
  { _clusters :: Ve Cluster
  , _featureNames :: Ve Text
  , _groupNames :: Ve Text
  , _syncOutFile :: Maybe FilePath
  , _hiFeatures :: S.Set Int
  } deriving (Show)

emptySt =
  St
    { _clusters = V.empty
    , _featureNames = V.empty
    , _groupNames = V.empty
    , _syncOutFile = Nothing
    , _hiFeatures = S.empty
    }

data Cluster = Cluster
  { _position :: V2 Float
  , _weight :: Float
  , _features :: Ve Float
  , _featMeans :: Ve Float
  , _featVars :: Ve Float
  , _clusterSelected :: Bool
  , _groups :: S.Set Int
  } deriving (Show)

emptyCluster =
  Cluster
    { _position = V2 0 0
    , _weight = 0
    , _features = V.empty
    , _featMeans = V.empty
    , _featVars = V.empty
    , _clusterSelected = False
    , _groups = S.empty
    }

$(makeLenses ''AppState)

$(makeLenses ''Cluster)

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
