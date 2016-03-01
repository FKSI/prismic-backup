{-# LANGUAGE OverloadedStrings #-}

module Http where

import Control.Lens
import Data.Aeson (eitherDecode)
import Data.Text (Text)
import Network.Wreq
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as S
import qualified Prismic as P

doQuery :: Session -> String -> IO (Either String P.Res)
doQuery sess q = do
    res <- S.get sess q
    putStrLn $ "[Fetched] " ++ q
    --putStrLn $ show res
    let body = res ^. responseBody
    --putStrLn $ show body
    return $ eitherDecode body
