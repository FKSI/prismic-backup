module Storage where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BIO (writeFile)
import qualified Data.List as L (stripPrefix)
import qualified Data.Maybe as M (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as IO (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (mapM)
import qualified Prismic as P
import System.Directory (createDirectoryIfMissing)
import System.FilePath (normalise, makeValid, dropFileName)

storeDocument :: FilePath -> P.Document -> IO ()
storeDocument outputDir document = do
    let docType = P._type document
    let dirPath = mkDirPath outputDir docType
    let id = P._id document
    let doc = P.serializeDocument document
    _ <- createDirectoryIfMissing True dirPath
    let filePath = mkFilePath dirPath id
    IO.writeFile filePath doc
    putStrLn $ "[Written] [Document] " ++ filePath

mkDirPath :: FilePath -> String -> FilePath
mkDirPath outputDir docType = makeValid $ normalise path
    where
        path = outputDir ++ "/" ++ docType

mkFilePath :: FilePath -> String -> FilePath
mkFilePath outputDir id = makeValid $ normalise path
    where
        path = outputDir ++ "/" ++ id ++ ".json"

storeDocuments :: FilePath -> Vector P.Document -> IO ()
storeDocuments outputDir docs = do
    _ <- V.mapM (\d -> storeDocument outputDir d) docs
    return ()

storeResource :: FilePath -> String -> ByteString -> IO ()
storeResource outputDir url d = do
    let (filePath, dirPath) = mkResourcePath outputDir url
    _ <- createDirectoryIfMissing True dirPath
    BIO.writeFile filePath d
    putStrLn $ "[Written] [Resource] " ++ filePath

mkResourcePath :: FilePath -> String -> (FilePath, FilePath)
mkResourcePath outputDir url = (fullPath, dropFileName fullPath)
    where
        withoutProtocol = M.fromMaybe url $ orElse (L.stripPrefix "https://" url) (L.stripPrefix "http://" url)
        fullPath = makeValid $ normalise $ outputDir ++ "/" ++ withoutProtocol

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y
