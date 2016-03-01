module Storage where

import Data.Text (Text)
import qualified Data.Text.IO as IO (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (mapM)
import qualified Prismic as P
import System.Directory (createDirectoryIfMissing)
import System.FilePath (normalise, makeValid)

storeDocument :: FilePath -> String -> (String, Text) -> IO ()
storeDocument outputDir docType (id, doc) = do
    let dirPath = mkDirPath outputDir docType
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

storeDocuments :: FilePath -> String -> Vector (String, Text) -> IO ()
storeDocuments outputDir docType docs = do
    _ <- V.mapM (\d -> storeDocument outputDir docType d) docs
    return ()
