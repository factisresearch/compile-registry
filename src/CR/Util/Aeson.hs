module CR.Util.Aeson
    ( aesonOpts
    )
where

import Data.Aeson.Types
import Data.Char

aesonOpts :: Int -> Options
aesonOpts dropCount =
    defaultOptions
    { fieldLabelModifier = drop dropCount
    , constructorTagModifier = camelTo2 '_'
    , allNullaryToStringTag = True
    , omitNothingFields = False
    , sumEncoding = ObjectWithSingleField
    }
