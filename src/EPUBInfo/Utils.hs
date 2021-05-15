-- | Utilities and helpers
module EPUBInfo.Utils where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBC8
import Protolude

-- | Print an JSON-serializable object to the standard output.
printJson :: ToJSON a => a -> IO ()
printJson = LBC8.putStrLn . A.encode
