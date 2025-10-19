{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config where

import St

import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import Options.Applicative

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.List (foldl', foldl1')
import Data.Version (showVersion)
import DearImGui.Internal.Text (pack)
import GHC.Generics (Generic)
import Paths_clusterpainter (version)
import SDL.Vect (V2(..))

data Shape = Shape
  { projection :: [(Float, Float)]
  , topology :: [[Float]]
  } deriving (Show, Generic, FromJSON, ToJSON)

data AnnotOpts = AnnotOpts
  { annFeatureNames :: Maybe FilePath
  , annGroupNames :: Maybe FilePath
  , annGroups :: Maybe FilePath
  } deriving (Show)

annotOpts :: Parser AnnotOpts
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
  , outFile :: Maybe FilePath
  , uiFontSize :: Float
  } deriving (Show)

opts :: Parser Opts
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
               "topology description (should contain an object with key `projection' which specifies 2D position of each of the clusters, and `topology' which specifies a squared distance of clusters to each other cluster)"
  weightsFile <-
    optional . strOption
      $ short 'w'
          <> long "weights"
          <> metavar "JSON"
          <> help "cluster weights (typically member counts)"
  varsFile <-
    Right
      <$> strOption
            (short 'v'
               <> long "variances"
               <> metavar "JSON"
               <> help "read in-cluster variance values from this file")
      <|> Left
            <$> option
                  auto
                  (short 'V'
                     <> long "default-variance"
                     <> metavar "FLOAT"
                     <> help
                          "assume this variance in all clusters and all dimensions"
                     <> value 1.0
                     <> showDefault)
  meansFile <-
    optional . strOption
      $ short 'm'
          <> long "means"
          <> metavar "JSON"
          <> help
               "read in-cluster mean values from this file (if missing, means are assumed identical to the feature values)"
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
          <> help "synchronize output into this file"
  uiFontSize <-
    option auto
      $ short 'A'
          <> long "font-size"
          <> metavar "PIXELS"
          <> help "UI font size for rendering"
          <> value 20.0
          <> showDefault
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

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile path = do
  x' <- eitherDecodeFileStrict path
  case x' of
    Left err -> fail $ "failed reading " ++ path ++ ": " ++ err
    Right x -> pure x

checkMtxSz ::
     (Foldable t, MonadFail m) => String -> Int -> Int -> t (t a) -> m ()
checkMtxSz what n nn xs =
  unless (length xs == n && all ((== nn) . length) xs) . fail
    $ "bad array size in "
        ++ what
        ++ " (expected "
        ++ show n
        ++ " arrays of length "
        ++ show nn
        ++ ")"

checkVecSz :: (Foldable t, MonadFail m) => String -> Int -> t a -> m ()
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
  topoData <- decodeFile (topoFile o)
  let projs = map (uncurry V2) $ projection topoData
      topos = topology topoData
  checkVecSz (topoFile o ++ ", key projections") nc projs
  checkMtxSz (topoFile o ++ ", key topology") nc nc topos
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
  filestate <-
    case inputFiles o of
      Right inf -> do
        x <- decodeFile inf
        checkVecSz (inf ++ " groups") nc $ fsClusterGroups x
        checkVecSz (inf ++ " feature names") nd $ fsFeatures x
        let ngs = length (fsClusterGroups x)
        unless (and [i >= 0 && i < ngs | is <- fsClusterGroups x, i <- is])
          $ fail "group index out of range"
        pure x
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
              pure (gns, replicate nc [])
            (Nothing, Just gf) -> do
              gs <- decodeFile gf
              checkVecSz gf nc gs
              let ngs = max 0 . succ . maximum $ concat gs
                  checkGroups = filter $ liftA2 (&&) (>= 0) (< ngs)
              pure (map ((++) "group " . show) [1 .. ngs], map checkGroups gs)
            (Just gnf, Just gf) -> do
              gns <- decodeFile gnf
              gs <- decodeFile gf
              checkVecSz gf nc gs
              let ngs = length gns
                  checkGroups = filter $ liftA2 (&&) (>= 0) (< ngs)
              pure (gns, map checkGroups gs)
        pure FSt {fsClusterGroups = gs, fsFeatures = fns, fsGroups = gns}
  let featureMins = foldl1' (zipWith min) fs
      featureMaxs = foldl1' (zipWith max) fs
      inf = 1 / 0 :: Float
      posRng =
        foldl'
          (\(l, u) p -> (liftA2 min l p, liftA2 max u p))
          (pure inf, pure (-inf))
          projs
  pure
    emptySt
      { _featureNames = V.fromList . map pack $ fsFeatures filestate
      , _groupNames = V.fromList . map pack $ fsGroups filestate
      , _syncOutFile = outFile o
      , _clusters =
          V.fromList
            $ zipClusters projs topos ws fs ms vs (fsClusterGroups filestate)
      , _featureRanges =
          V.fromList
            $ zipWith V2 featureMins (zipWith (-) featureMaxs featureMins)
      , _positionRange = posRng
      , _fontSize = uiFontSize o
      }

zipClusters ::
     [V2 Float]
  -> [[Float]]
  -> [Float]
  -> [[Float]]
  -> [[Float]]
  -> [[Float]]
  -> [[Int]]
  -> [Cluster]
zipClusters (p:ps) (t:ts) (w:ws) (f:fs) (m:ms) (v:vs) (g:gs) =
  emptyCluster
    { _position = p
    , _topoDists = V.fromList t
    , _weight = w
    , _features = V.fromList f
    , _featMeans = V.fromList m
    , _featVars = V.fromList v
    , _groups = S.fromList g
    }
    : zipClusters ps ts ws fs ms vs gs
zipClusters _ _ _ _ _ _ _ = []
