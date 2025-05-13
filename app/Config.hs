{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config where

import St

import Control.Monad
import Data.Aeson
import Data.Bool
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Version (showVersion)
import GHC.Generics
import Options.Applicative
import Paths_clusterpainter (version)

data Shape = Shape
  { projection :: [(Float, Float)]
  , topology :: [[Float]]
  } deriving (Show, Generic, FromJSON, ToJSON)

data AnnotOpts = AnnotOpts
  { annFeatureNames :: Maybe FilePath
  , annGroupNames :: Maybe FilePath
  , annGroups :: Maybe FilePath
  } deriving (Show)

annotOpts = do
  annFeatureNames <-
    optional . strOption
      $ short 'F'
          <> long "feature-names"
          <> metavar "JSON"
          <> help "read the names of features from this file"
  annGroupNames <-
    optional . strOption
      $ short 'G'
          <> long "group-names"
          <> metavar "JSON"
          <> help "read group names from this file"
  annGroups <-
    optional . strOption
      $ short 'I'
          <> long "groups"
          <> metavar "JSON"
          <> help "read group memberships from this file"
  pure AnnotOpts {..}

data Opts = Opts
  { featuresFile :: FilePath
  , topoFile :: FilePath
  , weightsFile :: Maybe FilePath
  , varsFile :: Either Float FilePath
  , meansFile :: Maybe FilePath
  , inputFiles :: Either AnnotOpts FilePath
  , inFile :: Maybe FilePath
  , outFile :: Maybe FilePath
  } deriving (Show)

opts = do
  featuresFile <-
    strOption
      $ short 'f'
          <> long "features"
          <> metavar "JSON"
          <> help "read feature values from this file"
  topoFile <-
    strOption
      $ short 't'
          <> long "topology"
          <> metavar "JSON"
          <> help
               "topology description (should contain an object with key `projection' which specifies 2D position of each of the clusters)"
  weightsFile <-
    optional . strOption
      $ short 'w'
          <> long "weights"
          <> metavar "JSON"
          <> help "cluster weights (typically member counts)"
  varsFile <-
    Left
      <$> option
            auto
            (short 'V'
               <> long "default-variance"
               <> metavar "FLOAT"
               <> help "assume this variance in all clusters and all dimensions")
      <|> Right
            <$> strOption
                  (short 'v'
                     <> long "variances"
                     <> metavar "JSON"
                     <> help "read in-cluster variance values from this file")
  meansFile <-
    optional . strOption
      $ short 'm'
          <> long "means"
          <> metavar "JSON"
          <> help "read in-cluster mean values from this file"
  inputFiles <-
    Right
      <$> strOption
            (short 'i'
               <> long "input"
               <> metavar "JSON"
               <> help "read previous output of clusterpainter")
      <|> Left <$> annotOpts
  outFile <-
    optional . strOption
      $ short 'o'
          <> long "output"
          <> metavar "JSON"
          <> help "periodically save output into this file"
  pure Opts {..}

parseOpts :: IO Opts
parseOpts =
  customExecParser (prefs $ showHelpOnEmpty)
    $ info
        (opts <**> helper <**> simpleVersioner (showVersion version))
        (fullDesc
           <> header "clusterpainter -- fast metaclustering annotator"
           <> (footer
                 $ "Copyright (c) University of Luxembourg."
                     ++ " clusterpainter is developed at Luxembourg Centre for Systems Biomedicine,"
                     ++ " and distributed under the terms of Apache-2.0 license."
                     ++ " See https://github.com/LCSB-BioCore/clusterpainter for details and source."))

decodeFile path = do
  x <- eitherDecodeFileStrict path
  case x of
    Left err -> fail $ "failed reading " ++ path ++ ": " ++ err
    Right x -> pure x

checkMtxSz what n nn xs =
  unless (length xs == n && all ((== nn) . length) xs) . fail
    $ "bad array size in "
        ++ what
        ++ " (expected "
        ++ show n
        ++ " arrays of length "
        ++ show nn
        ++ ")"

checkVecSz what n xs =
  unless (length xs == n) . fail
    $ "bad vector size in " ++ what ++ " (expected " ++ show n ++ ")"

processOpts :: IO AppState
processOpts = do
  o <- parseOpts
  fs <- decodeFile $ featuresFile o
  let nc = length fs
  unless (nc > 0) $ fail "input contains no clusters"
  let nd = length (head fs)
  checkMtxSz (featuresFile o) nc nd fs
  ws <-
    case weightsFile o of
      Nothing -> pure $ replicate nc 1
      Just wf -> do
        x <- decodeFile wf
        checkVecSz wf nc x
        pure x
  projs <- projection <$> decodeFile (topoFile o)
  checkVecSz (topoFile o) nc projs
  ms <-
    case meansFile o of
      Nothing -> pure fs
      Just mfn -> do
        x <- decodeFile mfn
        checkMtxSz mfn nc nd x
        pure x
  vs <-
    case varsFile o of
      Left v -> pure $ replicate nc $ replicate nd v
      Right vfn -> do
        x <- decodeFile vfn
        checkMtxSz vfn nc nd x
        pure x
  fst <-
    case inputFiles o of
      Right inf -> do
        fst <- decodeFile inf
        checkVecSz (inf ++ " groups") nc $ fsClusterGroups fst
        let ngs = length . head $ fsClusterGroups fst
        checkMtxSz (inf ++ " groups") nc ngs $ fsClusterGroups fst
        checkVecSz (inf ++ " feature names") nd $ fsFeatures fst
        checkVecSz (inf ++ " group names") ngs $ fsGroups fst
        pure fst
      Left ao -> do
        fns <-
          case annFeatureNames ao of
            Nothing -> pure $ map ((++) "feature " . show) [1 .. nd]
            Just ffn -> do
              x <- decodeFile ffn
              checkVecSz ffn nd x
              pure x
        (gns, gs) <-
          case (annGroupNames ao, annGroups ao) of
            (Nothing, Nothing) -> pure ([], replicate nc [])
            (Just gnf, Nothing) -> do
              gns <- decodeFile gnf
              pure (gns, replicate nc (replicate (length gns) False))
            (Nothing, Just gf) -> do
              gs <- decodeFile gf
              checkVecSz gf nc gs
              let ngs = length (head gs)
              checkMtxSz gf nc ngs gs
              pure (map ((++) "group " . show) [1 .. ngs], gs)
            (Just gnf, Just gf) -> do
              gns <- decodeFile gnf
              gs <- decodeFile gf
              checkMtxSz gf nc (length gns) gs
              pure (gns, gs)
        pure FSt {fsClusterGroups = gs, fsFeatures = fns, fsGroups = gns}
  pure
    St
      { featureNames = V.fromList $ fsFeatures fst
      , groupNames = V.fromList $ fsGroups fst
      , syncOutFile = outFile o
      , items = V.fromList $ zipClusters projs ws fs ms vs (fsClusterGroups fst)
      }

zipClusters (p:ps) (w:ws) (f:fs) (m:ms) (v:vs) (g:gs) =
  Cluster
    { position = p
    , weight = w
    , features = V.fromList f
    , featMeans = V.fromList m
    , featVars = V.fromList v
    , selected = False
    , groups = S.fromList [i | (b, i) <- zip g [1 ..], b]
    }
    : zipClusters ps ws fs ms vs gs
zipClusters _ _ _ _ _ _ = []
