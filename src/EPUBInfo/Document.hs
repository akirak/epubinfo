-- |
module EPUBInfo.Document
  ( -- * High-level operations
    getMetadata,
    getTableOfContents,
    EPUBTocItemMissing,
    getCoverImage,
    EPUBNoCoverImage,

    -- * Re-exports
    getArchiveContent,
    TableOfContents,
    ToTableOfContents (..),
    TocRenderOptions (..),
    tocToOrg,
    tocToMarkdown,
    module EPUBInfo.Document.Opf,
    module EPUBInfo.Document.Container,
    module EPUBInfo.Document.Nav,
  )
where

import Control.Monad.Catch (MonadThrow (..))
import EPUBInfo.Document.Container
import EPUBInfo.Document.Nav
import EPUBInfo.Document.Ncx
import EPUBInfo.Document.Opf
import EPUBInfo.Monad
import EPUBInfo.Toc
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
    Nothing -> do
      mNcxRelPath <- getNcxPathMaybe opfDoc
      case mNcxRelPath of
        Just ncxRelPath -> do
          let ncxPath = takeDirectory opfPath </> ncxRelPath
          ncxDocument <- readNcxDocument ncxPath
          toTableOfContents ncxDocument
        Nothing -> throwM EPUBTocItemMissing

data EPUBNoCoverImage = EPUBNoCoverImage
  deriving (Show, Typeable)

instance Exception EPUBNoCoverImage

-- | Return the media type and the file path of the cover image.
--
-- To get an actual content of the file, use @getArchiveContent@.
--
-- If the archive has no cover image, @EPUBNoCoverImage@ exception is
-- thrown.
getCoverImage :: EPUBM (Maybe Text, FilePath)
getCoverImage = do
  opfPath <- readContainer >>= getOpfPath
  opfDoc <- readOpfDocument opfPath
  mCover <- lookupCover opfDoc
  case mCover of
    Nothing -> throwM EPUBNoCoverImage
    Just (mMime, relPath) ->
      return (mMime, takeDirectory opfPath </> relPath)
