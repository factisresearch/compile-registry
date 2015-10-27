{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CR.InterfaceTypes where

-- TODO: this should be an extra package because clients need this

import CR.Util.Aeson

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype CpuArch
    = CpuArch { unCpuArch :: T.Text }
      deriving (Show, Eq, ToJSON, FromJSON)

newtype InputHash
    = InputHash { unInputHash :: T.Text }
      deriving (Show, Eq, ToJSON, FromJSON)

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
   = ResponseCached !ResponseFile
   | ResponseNotFound
     deriving (Show, Eq, Generic)

instance FromJSON Response where
    parseJSON = genericParseJSON (aesonOpts 0)

instance ToJSON Response where
    toJSON = genericToJSON (aesonOpts 0)

data ResponseFile
   = ResponseFile
   { rf_cpuArch :: !CpuArch
   , rf_inputHash :: !InputHash
   , rf_oFile :: !AsBase64
   , rf_hiFile :: !AsBase64
   } deriving (Show, Eq, Generic)

instance FromJSON ResponseFile where
    parseJSON = genericParseJSON (aesonOpts 3)

instance ToJSON ResponseFile where
    toJSON = genericToJSON (aesonOpts 3)

data UploadFile
   = UploadFile
   { uf_cpuArch :: !CpuArch
   , uf_inputHash :: !InputHash
   , uf_oFile :: !AsBase64
   , uf_hiFile :: !AsBase64
   , uf_buildTimeSeconds :: !Double
   } deriving (Show, Eq, Generic)

instance FromJSON UploadFile where
    parseJSON = genericParseJSON (aesonOpts 3)

instance ToJSON UploadFile where
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
