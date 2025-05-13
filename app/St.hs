{-# LANGUAGE OverloadedStrings #-}

module St where

import qualified Data.Set as S
import qualified Data.Vector as V

import Data.Aeson

type Ve = V.Vector

data AppState = St
  { clusters :: Ve Cluster
  , featureNames :: Ve String
  , groupNames :: Ve String
  , syncOutFile :: Maybe FilePath
  } deriving (Show)

data Cluster = Cluster
  { position :: (Float, Float)
  , weight :: Float
  , features :: Ve Float
  , featMeans :: Ve Float
  , featVars :: Ve Float
  , selected :: Bool
  , groups :: S.Set Int
  } deriving (Show)

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
