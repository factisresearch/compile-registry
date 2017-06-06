{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module CR.InterfaceTypes where

-- TODO: this should be an extra package because clients need this

import CR.Util.Aeson

import Web.Spock
import Web.Routing.Combinators
import GHC.Generics
import Data.Aeson
import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Hash as BFH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Bytes.Serial as SE
import qualified Data.Serialize as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype CpuArch
    = CpuArch { unCpuArch :: T.Text }
      deriving (Show, Eq, ToJSON, FromJSON,Generic)

instance SE.Serial CpuArch where

newtype InputHash
    = InputHash { unInputHash :: T.Text }
      deriving (Show, Eq, ToJSON, FromJSON)

-- | Denotes the kind of cached file e.g. o-File, hi.-File
type BuildFileType = T.Text

-- | Usual bytestrings, but with base64 aeson repr
newtype AsBase64
    = AsBase64 { unBase64 :: BS.ByteString }
      deriving (Show, Eq)

instance ToJSON AsBase64 where
    toJSON (AsBase64 bs) =
        String $ T.decodeUtf8 $ B64.encode bs

instance FromJSON AsBase64 where
    parseJSON =
        withText "AsBase64" $ \t ->
        case B64.decode (T.encodeUtf8 t) of
          Left err ->
              fail $ "Can not parse as base64: " ++ err
          Right ok ->
              return $ AsBase64 ok

data Request
   = Request
   { r_cpuArch :: !CpuArch
   , r_inputHash :: !InputHash
   } deriving (Show, Eq, Generic)

instance FromJSON Request where
    parseJSON = genericParseJSON (aesonOpts 2)

instance ToJSON Request where
    toJSON = genericToJSON (aesonOpts 2)

data Response
   = ResponseCached !(M.Map BuildFileType AsBase64)
   | ResponseNotFound
     deriving (Show, Eq, Generic)

instance FromJSON Response where
    parseJSON = genericParseJSON (aesonOpts 0)

instance ToJSON Response where
    toJSON = genericToJSON (aesonOpts 0)

data UploadFiles
   = UploadFiles
   { uf_cpuArch :: !CpuArch
   , uf_inputHash :: !InputHash
   , uf_files :: !(M.Map BuildFileType AsBase64)
   , uf_buildTimeSeconds :: !Double
   } deriving (Show, Eq, Generic)

instance FromJSON UploadFiles where
    parseJSON = genericParseJSON (aesonOpts 3)

instance ToJSON UploadFiles where
    toJSON = genericToJSON (aesonOpts 3)

data UploadResponse
   = UploadOkay
   | UploadOkayDuplicate
   | UploadError
     deriving (Show, Eq, Generic)

instance FromJSON UploadResponse where
    parseJSON = genericParseJSON (aesonOpts 0)

instance ToJSON UploadResponse where
    toJSON = genericToJSON (aesonOpts 0)

loadEntryEndpoint :: Path '[] Open
loadEntryEndpoint = "v1" <//> "load-files"

storeEntryEndpoint :: Path '[] Open
storeEntryEndpoint = "v1" <//> "upload-files"

loadBloomEndpoint :: Path '[] Open
loadBloomEndpoint = "v1" <//> "load-bloom"

data BloomRequest
   = BloomRequest
   { br_cpuArch :: !CpuArch
   } deriving (Show, Eq, Generic)

instance FromJSON BloomRequest where
    parseJSON = genericParseJSON (aesonOpts 3)

instance ToJSON BloomRequest where
    toJSON = genericToJSON (aesonOpts 3)

data BloomResponse
   = BloomResponseOk !BloomResponseData
   | BloomResponseFailed
     deriving (Show, Eq, Generic)

instance FromJSON BloomResponse where
    parseJSON = genericParseJSON (aesonOpts 0)

instance ToJSON BloomResponse where
    toJSON = genericToJSON (aesonOpts 0)

data BloomResponseData
   = BloomResponseData
   { bre_cpuArch :: !CpuArch
   , bre_data :: !AsBase64
   } deriving (Show, Eq, Generic)

instance FromJSON BloomResponseData where
    parseJSON = genericParseJSON (aesonOpts 4)

instance ToJSON BloomResponseData where
    toJSON = genericToJSON (aesonOpts 4)

type BloomFilter = BF.Bloom InputHash

-- | Bloomfilter size in bits, MUST be equal on server and client (!)
bloomFilterSize :: Int
bloomFilterSize = 1014

-- | Bloomfilter hash function, must be equal on server and client (!)
bloomFilterHash :: InputHash -> [BF.Hash]
bloomFilterHash (InputHash txt) = BFH.cheapHashes 2 (T.unpack txt)

emptyBloomFilter :: BloomFilter
emptyBloomFilter = BF.empty bloomFilterHash bloomFilterSize

bloomFilterFromList :: [InputHash] -> BloomFilter
bloomFilterFromList = BF.fromList bloomFilterHash bloomFilterSize

serializeBloomFilter :: CpuArch -> BloomFilter -> BloomResponseData
serializeBloomFilter arch bf =
    BloomResponseData
    { bre_cpuArch = arch
    , bre_data = AsBase64 $ S.encode $ BF.bitArray bf
    }

parseBloomFilter :: Monad m => BloomResponseData -> m BloomFilter
parseBloomFilter bre =
    do bitArray <-
           case S.decode (unBase64 $ bre_data bre) of
             Left err ->
                 fail err
             Right ok ->
                 return ok
       return $ emptyBloomFilter { BF.bitArray = bitArray }

bloomContains :: InputHash -> BloomFilter -> Bool
bloomContains = BF.elem
