{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Prismic where

import Control.Lens
import Data.Aeson (Value, Array, Object, FromJSON, ToJSON, Result(..), encode, fromJSON, parseJSON, toJSON)
import Data.Aeson.Types (genericParseJSON, genericToJSON, defaultOptions, fieldLabelModifier)
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
    _id :: String,
    _data :: Object,
    _href :: String,
    _type :: String,
    _slugs :: Value,
    _tags :: Value,
    _linked_documents :: Value,
    _uid :: Value }
    deriving (Generic)

instance ToJSON Document where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Document where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

extractDocuments :: Res -> Vector Document
extractDocuments res = V.concatMap parseDocument $ results res
    where
        parseDocument r = case parseData r of
            Nothing -> V.empty
            Just d -> V.singleton d
        parseData :: Value -> Maybe Document
        parseData r = resultToMaybe $ fromJSON r

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Error _) = Nothing
resultToMaybe (Success a) = Just a

serializeDocument :: Document -> Text
serializeDocument = decodeUtf8 . toStrict . encode
