module EPUBInfo
  ( withEPUBFile,
    ZipExtractError (..),
    module EPUBInfo.Monad,
    module EPUBInfo.Document,

    -- * Re-exporting utilities for your convenience
    printJson,
  )
where

import qualified Codec.Archive.Zip as Z
import Control.Monad.Catch
import qualified Data.ByteString.Lazy as LB
import EPUBInfo.Document
import EPUBInfo.Monad
import EPUBInfo.Utils (printJson)
import Protolude
import Prelude (String)

data ZipExtractError = ZipExtractError FilePath String
  deriving (Typeable, Show)

instance Exception ZipExtractError

-- | Perform a certain operation on an EPUB file.
withEPUBFile :: FilePath -> EPUBM a -> IO a
withEPUBFile srcFile m = do
  lbs <- LB.readFile srcFile
  case Z.toArchiveOrFail lbs of
    Right archive -> runEPUBM m archive
    Left msg -> throwM $ ZipExtractError srcFile msg
