{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CR.Client where

import Network.Wreq

import Control.Monad
import qualified Data.Traversable as T
import Control.Lens

import Data.Aeson (toJSON)
import GHC.Generics
import Data.Time.Clock
import CR.InterfaceTypes
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory
import System.Process
import qualified Data.Text as T

data BuildStep
   = BuildStep
   { bs_cmdLine :: !String
   -- ^ System command to perform build step
   , bs_dependencies :: !(S.Set FilePath)
   -- ^ Files this build step depends on
   , bs_expectedOutputFiles :: !(M.Map BuildFileType FilePath)
   -- ^ Files this build step will produce
   , bs_additionalArgs :: !(M.Map T.Text T.Text)
   -- ^ Key-Value-pairs the result of this build step depends on, e.g. architecture
   -- compiler version
   , bs_name :: !String
   -- ^ Name for the Buildstep
   , bs_version :: !Int
   -- ^ Version of the Buildstep
   , bs_cpuArch :: !CpuArch
   }
   deriving (Show, Eq, Generic)

data ClientArgs
    = ClientArgs
    { c_bs :: BuildStep
    -- ^ Build-step we want to try to cache
    , c_bloomFilter :: FilePath
    -- ^ Bloom-Filter with an upper-approximation of all cached build steps
    , c_url :: String
    -- ^ URL to the Compile Registry
    }

computeHash :: BuildStep -> IO InputHash
computeHash _bs = undefined -- hash dependecies, cmdline and additionalArgs -- here

trackCmdTime :: IO () -> IO Double
trackCmdTime cmd =
    do start <- getCurrentTime
       cmd
       end <- getCurrentTime
       return $ realToFrac $ diffUTCTime end start

client :: ClientArgs -> IO ()
client args =
    do inputHash <- computeHash (c_bs args)
       -- check bloom filter here
       response <- post (c_url args ++ "/load-data") $ toJSON $
           Request
           { r_inputHash = inputHash
           , r_cpuArch = bs_cpuArch $ c_bs args
           }
       responseJ <- asJSON response
       let expectedOutputFiles = bs_expectedOutputFiles $ c_bs args
       case responseJ ^. responseBody of
         ResponseCached files
             | (M.keysSet files) == (M.keysSet expectedOutputFiles) ->
                 forM_ (M.keysSet files) $ \ft ->
                     BS.writeFile (expectedOutputFiles M.! ft) (unBase64 $ files M.! ft)
             | otherwise -> fail "Set of cached files distinct from expected list."
         ResponseNotFound -> buildStepNotCached inputHash (c_url args) (c_bs args)

buildStepNotCached :: InputHash -> String -> BuildStep -> IO ()
buildStepNotCached inputHash url bs =
    do time <- trackCmdTime $ callCommand (bs_cmdLine bs)
       -- TODO: Would be cool - to do this in a seperate thread
       forM_ (bs_expectedOutputFiles bs) $ \fp ->
           do doesExist <- doesFileExist fp
              unless doesExist $ fail $ concat ["Expected output file: ", fp, " was missing!"]
       files <- flip T.mapM (bs_expectedOutputFiles bs) $ liftM AsBase64 . BS.readFile
       _ <- post (url ++ "/upload-files") $ toJSON $
           UploadFiles
           { uf_inputHash = inputHash
           , uf_buildTimeSeconds = time
           , uf_files = files
           , uf_cpuArch = bs_cpuArch bs
           }
       return ()
