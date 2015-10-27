module CR.Types where

import qualified Data.ByteString as BS

data Config
   = Config
   { c_pgConnStr :: !BS.ByteString
   , c_port :: !Int
   } deriving (Show, Eq)
