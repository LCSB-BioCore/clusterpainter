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
  , _swSelect :: Bool
  , _showWeights :: Bool
  , _fontSize :: Float
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
    , _swSelect = False
    , _showWeights = False
    , _fontSize = 20
    }

data RendererData = RD
  { _rdFlatProgram :: GLuint
  , _rdStarProgram :: GLuint
  , _rdCircleArray :: GLuint
  , _flatProjection :: [Float] -> IO ()
  , _starProjection :: [Float] -> IO ()
  , _flatSize :: Float -> IO ()
  , _starSize :: Float -> IO ()
  , _flatPos :: Float -> Float -> IO ()
  , _starPos :: Float -> Float -> IO ()
  , _flatColor :: Float -> Float -> Float -> Float -> IO ()
  , _starSlices :: Int -> IO ()
  , _starSizes :: [Float] -> IO ()
  , _starColors :: [Float] -> IO ()
  }

instance Show RendererData where
  show _ = "undefined"

emptyRD =
  RD
    { _rdFlatProgram = 0
    , _rdStarProgram = 0
    , _rdCircleArray = 0
    , _flatProjection = \_ -> pure ()
    , _starProjection = \_ -> pure ()
    , _flatSize = \_ -> pure ()
    , _starSize = \_ -> pure ()
    , _flatPos = \_ _ -> pure ()
    , _starPos = \_ _ -> pure ()
    , _flatColor = \_ _ _ _ -> pure ()
    , _starSlices = \_ -> pure ()
    , _starSizes = \_ -> pure ()
    , _starColors = \_ -> pure ()
    }

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
  { fsClusterGroups :: [[Int]]
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

doOutput :: AppState -> IO ()
doOutput st =
  let fst =
        FSt
          { fsFeatures = st ^.. featureNames . each . to unpack
          , fsGroups = st ^.. groupNames . each . to unpack
          , fsClusterGroups = st ^.. clusters . each . groups . to S.toList
          }
   in case st ^. syncOutFile of
        Just fn -> encodeFile fn fst
        _ -> pure ()
