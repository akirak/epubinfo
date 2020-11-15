-- |
module EbookMetadata.EPUB
  ( readEPUBFile,
    parseEPUBStream,
    module EbookMetadata.EPUB.Types,
  )
where

import qualified Data.ByteString.Lazy as LB
import EbookMetadata.EPUB.Parse
import EbookMetadata.EPUB.Types
import Protolude

readEPUBFile :: FilePath -> IO EPUB
readEPUBFile srcFile =
  LB.readFile srcFile
    >>= either throwIO return . parseEPUBStream
