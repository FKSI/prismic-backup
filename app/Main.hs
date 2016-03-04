{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import qualified Data.Maybe as M (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (empty, union, unions)
import qualified Prismic as P
import qualified Http as H
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as WS
import Options.Generic
import qualified Storage as S
import qualified Text.Regex as R

data Config = Config {
    endpoint :: String,
    ref:: String,
    docTypes :: String,
    output :: Maybe FilePath }
    deriving (Show, Generic)

instance ParseRecord Config

main :: IO ()
main = WS.withAPISession $ \sess -> do
    config <- getRecord "Prismic Backup"
    let cfg = (config :: Config)
    let outputDir = M.fromMaybe defaultOutputDir (output cfg)
    let documentTypes = R.splitRegex (R.mkRegex ",") $ docTypes cfg
    let mkQuery = P.Query (endpoint cfg) (ref cfg)
    rc <- mapM (\dt -> fetchDocuments sess outputDir Set.empty $ Left $ mkQuery dt) documentTypes
    let resources = Set.unions rc
    --putStrLn $ show resources
    mapM_ (fetchResource sess outputDir) resources

fetchDocuments :: Session -> FilePath -> Set String -> Either P.Query String -> IO (Set String)
fetchDocuments sess outputDir resources target = do
    let url = case target of Left q -> P.endpoint q ++ P.queryString q
                             Right u -> u
    r <- H.doQuery sess url
    case r of
        Left e -> error e
        Right res -> do
            let docs = P.extractDocuments res
            --putStrLn $ show docs
            let newResources = Set.union resources $ P.analyzeDocuments docs
            S.storeDocuments outputDir docs
            let next = P.next_page res
            case next of Nothing -> return newResources
                         Just u -> fetchDocuments sess outputDir newResources $ Right u

fetchResource :: Session -> FilePath -> String -> IO ()
fetchResource sess outputDir url = do
    d <- H.doQueryResource sess url
    let resourcesDir = outputDir ++ "/_rc"
    mapM_ (S.storeResource resourcesDir url) d
    return ()

defaultOutputDir :: FilePath
defaultOutputDir = "./output"
