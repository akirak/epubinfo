{-# LANGUAGE OverloadedStrings #-}

-- | Parsing NCX documents which was supported in EPUB 2.
module EPUBInfo.Document.Ncx
  ( NcxDocument,
    readNcxDocument,
  )
where

import Control.Monad.Catch (MonadThrow (..))
import EPUBInfo.Monad
import EPUBInfo.Toc
import Protolude
import qualified Text.XML.Cursor as C
import Prelude (String)

newtype NcxDocument = NcxDocument C.Cursor
  deriving (Show)

readNcxDocument :: FilePath -> EPUBM NcxDocument
readNcxDocument path = NcxDocument . C.fromDocument <$> readXmlInArchive path

data NcxDocumentException
  = NcxElementMissing String
  | NcxLabelTextMissing
  | NcxLabelContentMissing
  | NavAttributeMissing String String
  deriving (Show, Typeable)

instance Exception NcxDocumentException

instance ToTableOfContents NcxDocument where
  toTableOfContents (NcxDocument root) =
    case root C.$// C.element "{http://www.daisy.org/z3986/2005/ncx/}navMap" of
      [] -> throwM $ NcxElementMissing "navMap"
      (navMapC : _) -> TableOfContents <$> mapM go (navMapC C.$/ navPoint)
    where
      go navPointC =
        TocNode
          <$> tocEntry navPointC
          <*> mapM go (navPointC C.$/ navPoint)

tocEntry :: MonadThrow m => C.Cursor -> m TocEntry
tocEntry navPointC =
  TocEntry
    <$> findTitle
    <*> findSrc
  where
    findTitle =
      case navPointC C.$/ navLabelText of
        [] -> throwM NcxLabelTextMissing
        (c : _) -> getLabelText c
    getLabelText c =
      case c C.$/ C.content of
        [] -> throwM NcxLabelContentMissing
        (text : _) -> return text
    findSrc =
      case navPointC C.$/ navContent >=> C.attribute "src" of
        [] -> throwM $ NavAttributeMissing "content" "src"
        -- This seems to be a path from the archive root, and not from
        -- the relative path from the ncx. It can be problematic if
        -- you use this value, since it is not consistent with the
        -- specification of the navigation document.
        (src : _) -> return src

navPoint :: C.Axis
navPoint = C.element "{http://www.daisy.org/z3986/2005/ncx/}navPoint"

navLabelText :: C.Axis
navLabelText =
  C.element "{http://www.daisy.org/z3986/2005/ncx/}navLabel"
    C.&/ C.element "{http://www.daisy.org/z3986/2005/ncx/}text"

navContent :: C.Axis
navContent = C.element "{http://www.daisy.org/z3986/2005/ncx/}content"
