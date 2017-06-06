{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module CR.Server where

import CR.Types
import CR.InterfaceTypes

import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe
import Data.Time.Clock
import Data.Default.Class
import Data.Functor.Contravariant
import Data.Monoid
import Data.Int
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Identity
import Web.Spock
import Web.Spock.Config
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Hasql.Connection as H
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Query as H
import qualified Hasql.Session as H
import qualified Hasql.Pool as HP
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HT

launchServer :: Config -> IO ()
launchServer cfg =
    do let poolSettings = (6, 30, c_pgConnStr cfg)
       pool <- HP.acquire poolSettings
       putStrLn "Setup the database..."
       result <-
           HP.use pool setupDatabase
       case result of
         Left err ->
             fail $ "Failed to setup databse: " ++ show err
         Right () ->
             putStrLn "Done."
       sessCfg <- defaultSpockCfg () PCNoDatabase ()
       runSpock (c_port cfg) $ spock sessCfg (spockApp pool)

runDbEndpoint :: (FromJSON req, ToJSON resp, MonadIO m) => HP.Pool -> resp -> (req -> H.Session resp) -> ActionT m ()
runDbEndpoint pool errorResp runner =
    do req <- jsonBody'
       res <-
           liftIO $ HP.use pool $ runner req
       case res of
         Left err ->
             do liftIO $ putStrLn $ "Something bad happened: " ++ show err
                json errorResp
         Right resp ->
             json resp

spockApp :: HP.Pool -> SpockM conn sess st ()
spockApp pool =
    do post loadEntryEndpoint $
           runDbEndpoint pool ResponseNotFound $ \req ->
               HT.transaction HT.Serializable HT.Write $ loadEntry req
       put storeEntryEndpoint $
           runDbEndpoint pool UploadError $ \req ->
               HT.transaction HT.Serializable HT.Write $ storeEntry req
       post loadBloomEndpoint $
           runDbEndpoint pool BloomResponseFailed $ \req ->
               HT.transaction HT.Serializable HT.Write $
               -- TODO: cache this, or compute periodically
               do bf <- computeBloomFilter (br_cpuArch req)
                  return $ BloomResponseOk $ serializeBloomFilter (br_cpuArch req) bf

keyParams :: HE.Params (InputHash, CpuArch)
keyParams =
    contramap (unInputHash . view _1) (HE.value HE.text) <>
    contramap (unCpuArch . view _2) (HE.value HE.text)

doesExistQuery :: H.Query (InputHash, CpuArch) Bool
doesExistQuery =
    let doesExistStmt =
            "SELECT count(*) FROM buildsteps WHERE input_hash = $1 AND cpu_arch = $2 LIMIT 1"
    in H.statement doesExistStmt keyParams isOne True

isOne :: HD.Result Bool
isOne = (== (1::Int64)) <$> HD.singleRow (HD.value def)

storeEntry :: UploadFiles -> HT.Transaction UploadResponse
storeEntry req =
    do let identTpl =
               (uf_inputHash req, uf_cpuArch req)
       exists <- HT.query identTpl doesExistQuery
       case exists of
         True ->
             do touchEntry identTpl
                return $ UploadOkayDuplicate
         False ->
             do let insertStmt =
                       "INSERT INTO buildsteps\n" <>
                       "       ( last_access, cpu_arch, input_hash, build_time_seconds )\n" <>
                       "VALUES ( NOW(), ?, ?, ? )"
                    insertDecoder :: HE.Params (CpuArch, InputHash, Double)
                    insertDecoder =
                        contramap (unCpuArch . view _1) (HE.value HE.text) <>
                        contramap (unInputHash . view _2) (HE.value HE.text) <>
                        contramap (picosecondsToDiffTime . round . (* 1e12) . view _3) (HE.value HE.interval)
                    insertQuery = H.statement insertStmt insertDecoder HD.unit True
                    insertFileStmt =
                        "INSERT INTO files\n" <>
                        "       ( cpu_arch, input_hash, file_type, file_contents )\n" <>
                        "VALUES ( ?, ?, ?, ?)"
                    insertFileDecoder :: HE.Params (CpuArch, InputHash, T.Text, BS.ByteString)
                    insertFileDecoder =
                        contramap (unCpuArch . view _1) (HE.value HE.text) <>
                        contramap (unInputHash . view _2) (HE.value HE.text) <>
                        contramap (view _3) (HE.value HE.text) <>
                        contramap (view _4) (HE.value HE.bytea)
                    insertFileQuery = H.statement insertFileStmt insertFileDecoder HD.unit True
                HT.query
                    ( uf_cpuArch req
                    , uf_inputHash req
                    , uf_buildTimeSeconds req
                    )
                    insertQuery
                forM_ (M.toList $ uf_files req) $ \(ft, content) ->
                   HT.query
                       ( uf_cpuArch req
                       , uf_inputHash req
                       , ft
                       , unBase64 content
                       )
                       insertFileQuery
                return $ UploadOkay

updateQuery :: H.Query (InputHash, CpuArch) ()
updateQuery =
    let updateStmt =
            "UPDATE buildsteps SET last_access = NOW()\n" <>
            "WHERE input_hash = ? AND cpu_arch = ?"
    in H.statement updateStmt keyParams HD.unit True

touchEntry :: (InputHash, CpuArch) -> HT.Transaction ()
touchEntry identTpl =
       HT.query identTpl updateQuery

loadEntry :: Request -> HT.Transaction Response
loadEntry req =
    do let identTpl =
               (r_inputHash req, r_cpuArch req)
           filesStatement =
               "SELECT file_type, file_contents\n" <>
               "FROM files\n" <>
               "WHERE input_hash = $1 AND cpu_arch = $2"
           filesDecoder = HD.rowsList $ (,) <$> HD.value def <*> HD.value def
           filesQuery = H.statement filesStatement keyParams filesDecoder True
       res <- HT.query identTpl doesExistQuery
       if res
       then do buildFiles <- HT.query identTpl filesQuery
               touchEntry identTpl
               return $ ResponseCached $ M.map AsBase64 $ M.fromList buildFiles
       else return ResponseNotFound

computeBloomFilter :: CpuArch -> HT.Transaction BloomFilter
computeBloomFilter arch =
    do let hashStatement =
               "SELECT input_hash\n" <>
               "FROM buildsteps\n" <>
               "WHERE cpu_arch = $1"
           hashQuery = H.statement hashStatement (contramap unCpuArch (HE.value def)) (HD.rowsList (InputHash <$> (HD.value def))) True
       hashes <- HT.query arch hashQuery
       return $! bloomFilterFromList hashes

setupDatabase :: H.Session ()
setupDatabase =
    HT.transaction HT.Serializable HT.Write $
    do HT.sql $
           "CREATE TABLE IF NOT EXISTS buildsteps\n" <>
           "( cpu_arch TEXT NOT NULL,\n" <>
           "  input_hash TEXT NOT NULL,\n" <>
           "  build_time_seconds FLOAT8 NOT NULL,\n" <>
           "  last_access TIMESTAMPTZ NOT NULL,\n" <>
           "  PRIMARY KEY (cpu_arch, input_hash)\n" <>
           ")"
       HT.sql $
           "CREATE TABLE IF NOT EXISTS files\n" <>
           "( cpu_arch TEXT NOT NULL,\n" <>
           "  input_hash TEXT NOT NULL,\n" <>
           "  file_type TEXT NOT NULL,\n" <>
           "  file_contents BYTEA NOT NULL,\n" <>
           "  PRIMARY KEY (cpu_arch, input_hash, file_type)\n" <>
           ")"
       HT.sql "CREATE INDEX IF NOT EXISTS s_last_acc ON buildsteps USING btree(last_access)"
