module Main where

import CR.Server
import CR.Types

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main =
    do args <- getArgs
       case args of
         [portStr, pgStr] ->
             launchServer (Config (T.encodeUtf8 $ T.pack pgStr) (read portStr))
         _ ->
             putStrLn "Usage: compile-registry PORT PG_CONN_STR"
