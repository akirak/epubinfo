-- |
module EPUBInfo.Document
  ( -- * High-level operations
    getMetadata,
    getTableOfContents,

    -- * Re-exports
    module EPUBInfo.Document.Opf,
    module EPUBInfo.Document.Container,
    module EPUBInfo.Document.Nav,
  )
where

import EPUBInfo.Document.Container
import EPUBInfo.Document.Nav
import EPUBInfo.Document.Opf
import EPUBInfo.Monad
import Protolude
import System.FilePath (takeDirectory, (</>))

-- | Get metadata of the EPUB file.
getMetadata :: EPUBM EPUBMetadata
getMetadata =
  readContainer
    >>= getOpfPath
    >>= readOpfDocument
    >>= getMetadataFromOpf

getTableOfContents :: EPUBM TableOfContents
getTableOfContents = do
  opfPath <- readContainer >>= getOpfPath
  opfDoc <- readOpfDocument opfPath
  navRelPath <- getNavDocumentPath opfDoc
  let navPath = takeDirectory opfPath </> navRelPath
  navDocument <- readNavDocument navPath
  toTableOfContents navDocument
