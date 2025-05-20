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
import Graphics.GL
import SDL.Vect

type Ve = V.Vector

data AppState = St
  { _clusters :: Ve Cluster
  , _featureNames :: Ve Text
  , _groupNames :: Ve Text
  , _syncOutFile :: Maybe FilePath
  , _hiFeatures :: S.Set Int
  , _hiGroups :: S.Set Int
  , _featureRanges :: Ve (V2 Float)
  , _positionRange :: (V2 Float, V2 Float)
  , _rendererData :: RendererData
  , _painting :: Maybe Bool
  , _hover :: Maybe Int
  , _swSigma :: Float
  , _swMode :: SWMode
  } deriving (Show)

data SWMode
  = SWOff
  | SWTopo
  | SWAllFeatures
  | SWSelFeatures
  deriving (Show, Eq)

emptySt =
  St
    { _clusters = V.empty
    , _featureNames = V.empty
    , _groupNames = V.empty
    , _syncOutFile = Nothing
    , _hiFeatures = S.empty
    , _hiGroups = S.empty
    , _featureRanges = V.empty
    , _positionRange = (V2 0 0, V2 0 0)
    , _rendererData = emptyRD
    , _painting = Nothing
    , _hover = Nothing
    , _swSigma = 1
    , _swMode = SWOff
    }

data RendererData = RD
  { _rdProgram :: GLuint
  , _rdCircleArr :: GLuint
  } deriving (Show)

emptyRD = RD {_rdProgram = 0, _rdCircleArr = 0}

data Cluster = Cluster
  { _position :: V2 Float
  , _topoDists :: Ve Float
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
    , _topoDists = V.empty
    , _weight = 0
    , _features = V.empty
    , _featMeans = V.empty
    , _featVars = V.empty
    , _clusterSelected = False
    , _groups = S.empty
    }

$(makeLenses ''AppState)

$(makeLenses ''RendererData)

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
