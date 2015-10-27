{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module CR.Server where

import CR.Types
import CR.InterfaceTypes

import Data.Maybe
import Data.Int
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Identity
import Web.Spock.Safe
import qualified Data.Text as T

import qualified Hasql as H
import qualified Hasql.Postgres as HP

launchServer :: Config -> IO ()
launchServer cfg =
    do poolSettings <-
           maybe (fail "Invalid pool settings") return $ H.poolSettings 6 30
       pool <- H.acquirePool (HP.StringSettings (c_pgConnStr cfg)) poolSettings
       putStrLn "Setup the database..."
       result <-
           H.session pool setupDatabase
       case result of
         Left err ->
             fail $ "Failed to setup databse: " ++ show err
         Right () ->
             putStrLn "Done."
       runSpock (c_port cfg) $ spockT id (spockApp pool)

spockApp :: H.Pool HP.Postgres -> SpockT IO ()
spockApp pool =
    do post "load-data" $
         do req <- jsonBody'
            res <-
                liftIO $ H.session pool $ H.tx Nothing $ loadEntry req
            case res of
              Left err ->
                  do liftIO $ putStrLn $ "Something bad happened: " ++ show err
                     json ResponseNotFound
              Right resp ->
                  json resp
       put "store-data" $
         do req <- jsonBody'
            res <-
                liftIO $ H.session pool $ H.tx Nothing $ storeEntry req
            case res of
              Left err ->
                  do liftIO $ putStrLn $ "Something bad happened: " ++ show err
                     json UploadError
              Right resp ->
                  json resp

storeEntry :: UploadFile -> H.Tx HP.Postgres s UploadResponse
storeEntry req =
    do let identTpl =
               (unInputHash $ uf_inputHash req, unCpuArch $ uf_cpuArch req)
           doesExist =
               [H.stmt|SELECT COUNT(id) FROM storage WHERE input_hash = ? AND cpu_arch = ?|]
       amount <-
           H.countEx (uncurry doesExist identTpl)
       if amount /= 0
       then do touchEntry identTpl
               return $ UploadOkayDuplicate
       else do let insertStmt =
                       [H.stmt|
                        INSERT INTO storage
                              ( last_access, cpu_arch, input_hash
                              , o_file, hi_file, build_time_seconds)
                        VALUES
                              ( NOW(), ?, ?, ?, ?, ? )
                        |]
               H.unitEx $
                  insertStmt (unCpuArch $ uf_cpuArch req)
                       (unInputHash $ uf_inputHash req)
                       (unBase64 $ uf_oFile req)
                       (unBase64 $ uf_hiFile req)
                       (uf_buildTimeSeconds req)
               return $ UploadOkay

touchEntry :: (T.Text, T.Text) -> H.Tx HP.Postgres s ()
touchEntry identTpl =
    do let stmtUpdate =
                        [H.stmt|
                          UPDATE storage SET last_access = NOW()
                          WHERE input_hash = ? AND cpu_arch = ?|]
       H.unitEx (uncurry stmtUpdate identTpl)

loadEntry :: Request -> H.Tx HP.Postgres s Response
loadEntry req =
    do let identTpl =
               (unInputHash $ r_inputHash req, unCpuArch $ r_cpuArch req)
           stmt =
               [H.stmt|
                 SELECT
                   cpu_arch, input_hash, o_file, hi_file
                 FROM storage
                 WHERE input_hash = ? AND cpu_arch = ? LIMIT 1|]
       res <- H.maybeEx (uncurry stmt identTpl)
       case res of
         Just (arch, hash, ofile, hifile) ->
             do let stmtUpdate =
                        [H.stmt|
                          UPDATE storage SET last_access = NOW()
                          WHERE input_hash = ? AND cpu_arch = ?|]
                H.unitEx (uncurry stmtUpdate identTpl)
                return $
                    ResponseCached $
                    ResponseFile
                    { rf_cpuArch = CpuArch arch
                    , rf_inputHash = InputHash hash
                    , rf_oFile = AsBase64 ofile
                    , rf_hiFile = AsBase64 hifile
                    }
         Nothing ->
             return ResponseNotFound

doesIndexExist :: T.Text -> H.Tx HP.Postgres s Bool
doesIndexExist idx =
    do (resultSet :: Maybe (Identity Int64)) <-
           H.maybeEx $ [H.stmt|SELECT 1
                            FROM pg_class c
                            JOIN pg_namespace n ON n.oid = c.relnamespace
                            WHERE c.relname = ?
                            AND n.nspname = 'public'
                       |] idx
       return (isJust resultSet)

setupDatabase :: H.Session HP.Postgres IO ()
setupDatabase =
    H.tx Nothing $
    do H.unitEx
            [H.stmt|
              CREATE TABLE IF NOT EXISTS storage
              ( id SERIAL NOT NULL,
                cpu_arch TEXT NOT NULL,
                input_hash TEXT NOT NULL,
                build_time_seconds FLOAT8 NOT NULL,
                last_access TIMESTAMPTZ NOT NULL,
                o_file BYTEA NOT NULL,
                hi_file BYTEA NOT NULL,
                PRIMARY KEY (id)
              )
            |]
       isThere <- doesIndexExist "s_cpu"
       unless isThere $ H.unitEx [H.stmt|CREATE INDEX s_cpu ON storage USING btree(cpu_arch)|]
       isThere2 <- doesIndexExist "s_hash"
       unless isThere2 $ H.unitEx [H.stmt|CREATE INDEX s_hash ON storage USING btree(input_hash)|]
       isThere3 <- doesIndexExist "s_last_acc"
       unless isThere3 $
              H.unitEx [H.stmt|CREATE INDEX s_last_acc ON storage USING btree(last_access)|]
