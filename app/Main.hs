{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import qualified Data.Maybe as M (fromMaybe)
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
main = WS.withSession $ \sess -> do
    config <- getRecord "Prismic Backup"
    let cfg = (config :: Config)
    let outputDir = M.fromMaybe defaultOutputDir (output cfg)
    let documentTypes = R.splitRegex (R.mkRegex ",") $ docTypes cfg
    let mkQuery = P.Query (endpoint cfg) (ref cfg)
    mapM_ (\dt -> fetchDocuments sess outputDir dt $ Left $ mkQuery dt) documentTypes

fetchDocuments :: Session -> FilePath -> String -> Either P.Query String -> IO ()
fetchDocuments sess outputDir documentType target = do
    let url = case target of Left q -> P.endpoint q ++ P.queryString q
                             Right u -> u
    r <- H.doQuery sess url
    case r of
        Left e -> putStrLn e
        Right res -> do
            let docs = P.extractDocuments res
            --putStrLn $ show docs
            S.storeDocuments outputDir documentType docs
            let next = P.next_page res
            case next of Nothing -> return ()
                         Just u -> fetchDocuments sess outputDir documentType $ Right u

defaultOutputDir :: FilePath
defaultOutputDir = "./output"
