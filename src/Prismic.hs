{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Prismic where

import Control.Lens
import Data.Aeson (Value, Array, FromJSON, Result(..), encode, fromJSON)
import Data.ByteString.Lazy (toStrict)
import qualified Data.List as L
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V (empty, singleton, concatMap)
import GHC.Generics
import Network.HTTP (urlEncode)
import Network.Wreq (Options, defaults, param)

data Query = Query {
    endpoint :: String,
    reference :: String,
    documentType :: String }
    deriving (Show)

queryString :: Query -> String
queryString q = "?" ++ L.intercalate "&" params
    where
        params = fmap (\(n, v) -> n ++ "=" ++ urlEncode v) [format, pageSize, ref, query]
        format = ("format", "json")
        pageSize = ("pageSize", "100")
        ref = ("ref", reference q)
        query = ("q", "[[:d=at(document.type,\"" ++ documentType q ++ "\")]]")

data Res = Res {
    page :: Int,
    results_per_page :: Int,
    results_size :: Int,
    total_results_size :: Int,
    total_pages :: Int,
    next_page :: Maybe String,
    prev_page :: Maybe String,
    results :: Array,
    version :: Text,
    license :: Text }
    deriving (Show, Generic, FromJSON)

data Document = Document {
    id :: String }
    deriving (Generic, FromJSON)

extractDocuments :: Res -> Vector (String, Text)
extractDocuments res = V.concatMap parseDocument $ results res
    where
        parseDocument r = case getId r of
            Nothing -> V.empty
            Just id -> V.singleton (id, getDocument r)
        getDocument = decodeUtf8 . toStrict . encode
        getId :: Value -> Maybe String
        getId r = resultToMaybe $ fmap Prismic.id $ fromJSON r

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Error _) = Nothing
resultToMaybe (Success a) = Just a
