-- |
module EPUB2JSON.Document
  ( -- * High-level operations
    getMetadata,

    -- * Re-exports
    module EPUB2JSON.Document.Opf,
    module EPUB2JSON.Document.Container,
  )
where

import EPUB2JSON.Document.Container
import EPUB2JSON.Document.Opf
import EPUB2JSON.Monad
import Protolude

-- | Get metadata of the EPUB file.
getMetadata :: EPUBM EPUBMetadata
getMetadata =
  readContainer
    >>= getOpfPath
    >>= readOpfDocument
    >>= getMetadataFromOpf
