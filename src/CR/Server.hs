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
import qualified Data.Map.Strict as M
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
    do post loadEntryEndpoint $
         do req <- jsonBody'
            res <-
                liftIO $ H.session pool $ H.tx Nothing $ loadEntry req
            case res of
              Left err ->
                  do liftIO $ putStrLn $ "Something bad happened: " ++ show err
                     json ResponseNotFound
              Right resp ->
                  json resp
       put storeEntryEndpoint $
         do req <- jsonBody'
            res <-
                liftIO $ H.session pool $ H.tx Nothing $ storeEntry req
            case res of
              Left err ->
                  do liftIO $ putStrLn $ "Something bad happened: " ++ show err
                     json UploadError
              Right resp ->
                  json resp

storeEntry :: UploadFiles -> H.Tx HP.Postgres s UploadResponse
storeEntry req =
    do let identTpl =
               (unInputHash $ uf_inputHash req, unCpuArch $ uf_cpuArch req)
           doesExist =
               [H.stmt|SELECT 1 FROM buildsteps WHERE input_hash = ? AND cpu_arch = ? LIMIT 1|]
       amount <-
           H.maybeEx (uncurry doesExist identTpl)
       if isOne amount
       then do touchEntry identTpl
               return $ UploadOkayDuplicate
       else do let insertStmt =
                       [H.stmt|
                        INSERT INTO buildsteps
                              ( last_access, cpu_arch, input_hash, build_time_seconds )
                        VALUES
                              ( NOW(), ?, ?, ? )
                        |]
               let insertFileStmt =
                        [H.stmt|
                         INSERT INTO files
                               ( cpu_arch, input_hash, file_type, file_contents )
                         VALUES
                               ( ?, ?, ?, ?)
                         |]
               H.unitEx $
                  insertStmt
                       (unCpuArch $ uf_cpuArch req)
                       (unInputHash $ uf_inputHash req)
                       (uf_buildTimeSeconds req)
               forM_ (M.toList $ uf_files req) $ \(ft, content) -> H.unitEx $
                  insertFileStmt
                       (unCpuArch $ uf_cpuArch req)
                       (unInputHash $ uf_inputHash req)
                       (ft)
                       (unBase64 $ content)
               return $ UploadOkay

touchEntry :: (T.Text, T.Text) -> H.Tx HP.Postgres s ()
touchEntry identTpl =
    do let stmtUpdate = [H.stmt|
                          UPDATE buildsteps SET last_access = NOW()
                          WHERE input_hash = ? AND cpu_arch = ?|]
       H.unitEx (uncurry stmtUpdate identTpl)

loadEntry :: Request -> H.Tx HP.Postgres s Response
loadEntry req =
    do let identTpl =
               (unInputHash $ r_inputHash req, unCpuArch $ r_cpuArch req)
           existsStatement =
               [H.stmt|
                 SELECT
                   1
                 FROM buildsteps
                 WHERE input_hash = ? AND cpu_arch = ?|]
           filesStatement =
               [H.stmt|
                 SELECT
                   file_type, file_contents
                 FROM files
                 WHERE input_hash = ? AND cpu_arch = ?|]
       res <- liftM isOne $ H.maybeEx (uncurry existsStatement identTpl)
       if res
       then do buildFiles <- H.listEx (uncurry filesStatement identTpl)
               let stmtUpdate =
                       [H.stmt|
                         UPDATE buildsteps SET last_access = NOW()
                         WHERE input_hash = ? AND cpu_arch = ?|]
               H.unitEx (uncurry stmtUpdate identTpl)
               return $ ResponseCached $ M.map AsBase64 $ M.fromList buildFiles
       else return ResponseNotFound

isOne :: Maybe (Identity Int64) -> Bool
isOne = isJust

doesIndexExist :: T.Text -> H.Tx HP.Postgres s Bool
doesIndexExist =
    liftM isOne . H.maybeEx .
        [H.stmt|
          SELECT 1
          FROM pg_class c
          JOIN pg_namespace n ON n.oid = c.relnamespace
          WHERE c.relname = ?
          AND n.nspname = 'public'|]

setupDatabase :: H.Session HP.Postgres IO ()
setupDatabase =
    H.tx Nothing $
    do H.unitEx
            [H.stmt|
              CREATE TABLE IF NOT EXISTS buildsteps
              ( cpu_arch TEXT NOT NULL,
                input_hash TEXT NOT NULL,
                build_time_seconds FLOAT8 NOT NULL,
                last_access TIMESTAMPTZ NOT NULL,
                PRIMARY KEY (cpu_arch, input_hash)
              )
            |]
       H.unitEx
            [H.stmt|
              CREATE TABLE IF NOT EXISTS files
              ( cpu_arch TEXT NOT NULL,
                input_hash TEXT NOT NULL,
                file_type TEXT NOT NULL,
                file_contents BYTEA NOT NULL,
                PRIMARY KEY (cpu_arch, input_hash, file_type)
              )
            |]
       isThere <- doesIndexExist "s_last_acc"
       unless isThere $
           H.unitEx [H.stmt|CREATE INDEX s_last_acc ON buildsteps USING btree(last_access)|]
