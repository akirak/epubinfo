-- |
module EPUBInfo.Document
  ( -- * High-level operations
    getMetadata,

    -- * Re-exports
    module EPUBInfo.Document.Opf,
    module EPUBInfo.Document.Container,
  )
where

import EPUBInfo.Document.Container
import EPUBInfo.Document.Opf
import EPUBInfo.Monad
import Protolude

-- | Get metadata of the EPUB file.
getMetadata :: EPUBM EPUBMetadata
getMetadata =
  readContainer
    >>= getOpfPath
    >>= readOpfDocument
    >>= getMetadataFromOpf
