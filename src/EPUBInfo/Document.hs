-- |
module EPUBInfo.Document
  ( -- * High-level operations
    getMetadata,
    getTableOfContents,
    EPUBTocItemMissing,

    -- * Re-exports
    module EPUBInfo.Document.Opf,
    module EPUBInfo.Document.Container,
    module EPUBInfo.Document.Nav,
  )
where

import Control.Monad.Catch (MonadThrow (..))
import EPUBInfo.Document.Container
import EPUBInfo.Document.Nav
import EPUBInfo.Document.Opf
import EPUBInfo.Monad
import Protolude
import System.FilePath (takeDirectory, (</>))
import Prelude (Show (..))

-- | Get metadata of the EPUB file.
getMetadata :: EPUBM EPUBMetadata
getMetadata =
  readContainer
    >>= getOpfPath
    >>= readOpfDocument
    >>= getMetadataFromOpf

data EPUBTocItemMissing = EPUBTocItemMissing
  deriving (Typeable)

instance Show EPUBTocItemMissing where
  show EPUBTocItemMissing =
    "The opf document contains neither a navigation document nor a ncx."

instance Exception EPUBTocItemMissing

getTableOfContents :: EPUBM TableOfContents
getTableOfContents = do
  opfPath <- readContainer >>= getOpfPath
  opfDoc <- readOpfDocument opfPath
  mNavRelPath <- getNavDocumentPathMaybe opfDoc
  case mNavRelPath of
    Just navRelPath -> do
      let navPath = takeDirectory opfPath </> navRelPath
      navDocument <- readNavDocument navPath
      toTableOfContents navDocument
    Nothing ->
      throwM EPUBTocItemMissing
