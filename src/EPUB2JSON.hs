-- |
module EPUB2JSON
  ( readEPUBFile,
    parseEPUBStream,
    module EPUB2JSON.Types,
  )
where

import qualified Data.ByteString.Lazy as LB
import EPUB2JSON.Parse
import EPUB2JSON.Types
import Protolude

readEPUBFile :: FilePath -> IO EPUB
readEPUBFile srcFile =
  LB.readFile srcFile
    >>= either throwIO return . parseEPUBStream
