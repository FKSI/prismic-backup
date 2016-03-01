{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import qualified Data.Text as T (pack)
import qualified Data.Maybe as M (fromMaybe)
import qualified Prismic as P
import qualified Http as H
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as WS
import Options.Generic
import qualified Storage as S

data Config = Config {
    endpoint :: String,
    ref:: String,
    docType :: String,
    output :: Maybe FilePath }
    deriving (Show, Generic)

instance ParseRecord Config

main :: IO ()
main = WS.withSession $ \sess -> do
    config <- getRecord "Prismic Backup"
    let cfg = (config :: Config)
    let documentType = docType cfg
    let query = P.Query (endpoint cfg) (T.pack $ ref cfg) (T.pack documentType)
    let outputDir = M.fromMaybe defaultOutputDir (output cfg)
    fetchDocuments sess outputDir documentType $ Left query

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
