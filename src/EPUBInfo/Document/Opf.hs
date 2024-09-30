{-# LANGUAGE DeriveGeneric #-}

module EPUBInfo.Document.Opf
  ( -- * Types
    EPUBMetadata (..),

    -- ** Exception types
    OpfElementNotFound (..),
    OpfAttributeNotFound (..),

    -- * Document
    OpfDocument,
    readOpfDocument,
    getMetadataFromOpf,
    getNavDocumentPathMaybe,
    getNcxPathMaybe,
    lookupCover,
  )
where

import Control.Monad.Catch
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (unpack)
import qualified Data.Text.Encoding as T
import EPUBInfo.Monad
import Protolude
import qualified Text.XML as X
import qualified Text.XML.Cursor as C
import Prelude (String)

-- | The scheme version of this library.
schemeVersion :: Text
schemeVersion = "0.2"

data EPUBMetadata = EPUBMetadata
  { jsonVersion :: Text,
    -- | The value of a dc:identifier entry marked as the unique-identifier
    uniqueIdentifier :: Maybe Text,
    -- | Key-value pairs of dc:identifier entries by opf:scheme key
    identifierMap :: Map Text Text,
    -- | The value of a first dc:title entry
    title :: Maybe Text,
    -- | The value of a first dc:description entry
    description :: Maybe Text,
    -- | The value of a first dc:language entry
    language :: Maybe Text,
    -- | The value of a first dc:creator entry
    creator :: Maybe Text,
    -- | The values of dc:contributor entries
    contributors :: [Text],
    -- | The value of a first dc:date entry
    date :: Maybe Text,
    -- | The values of dc:subject entries
    subjects :: S.Set Text,
    -- | The value of a first dc:publisher entry
    publisher :: Maybe Text,
    -- | meta elements by name/property
    meta :: Map Text Text,
    -- | JSON serialization of calibre:user_categories meta element
    calibreUserCategories :: Maybe (Map Text [Text]),
    -- | Media type of the cover image, if any
    coverMediaType :: Maybe Text,
    coverFileName :: Maybe FilePath
  }
  deriving (Generic)

instance ToJSON EPUBMetadata

newtype OpfDocument = OpfDocument C.Cursor
  deriving (Show)

readOpfDocument :: FilePath -> EPUBM OpfDocument
readOpfDocument path = OpfDocument . C.fromDocument <$> readXmlInArchive path

newtype OpfElementNotFound = OpfElementNotFound
  {opfElementNotFoundMissingElement :: String}
  deriving (Show, Typeable)

instance Exception OpfElementNotFound

data OpfAttributeNotFound = OpfAttributeNotFound
  { opfAttributeNotFoundContext :: String,
    opfAttributeNotFoundAttribute :: X.Name
  }
  deriving (Show, Typeable)

instance Exception OpfAttributeNotFound

getMetadataFromOpf :: (MonadThrow m) => OpfDocument -> m EPUBMetadata
getMetadataFromOpf (OpfDocument rootCursor) =
  case (opfPackage C.&/ opfMetadata) rootCursor of
    [] -> throwM $ OpfElementNotFound "metadata"
    (metadataC : _) -> do
      let uniqueIdentifierId' =
            listToMaybe $
              rootCursor C.$| C.attribute "unique-identifier"
      uniqueIdentifier' <-
        case uniqueIdentifierId' of
          Nothing -> return Nothing
          Just elementId ->
            case metadataC
              C.$/ ( dcIdentifierElement
                       >=> C.attributeIs "id" elementId
                   ) of
              [] -> throwM $ OpfElementNotFound ('#' : unpack elementId)
              (c : _) -> return $ listToMaybe $ c C.$/ C.content
      let titleList' = metadataC C.$/ titleList
          descriptionList' = metadataC C.$/ descriptionList
          creatorList' = metadataC C.$/ creatorList
          contributorList' = metadataC C.$/ contributorList
          languageList' = metadataC C.$/ languageList
          subjectList' = metadataC C.$/ subjectList
          dateList' = metadataC C.$/ dateList
          publisherList' = metadataC C.$/ publisherList
          meta' = M.fromList $ catMaybes $ metadataC C.$/ metaAlist
          calibreUserCategories' =
            case M.lookup "calibre:user_categories" meta' of
              Just t -> decodeJsonText t
              _ -> mempty
      mCover <-
        case M.lookup "cover" meta' of
          Nothing -> return Nothing
          Just coverId -> Just <$> lookupCoverById coverId rootCursor
      return
        EPUBMetadata
          { jsonVersion = schemeVersion,
            uniqueIdentifier = uniqueIdentifier',
            identifierMap =
              M.fromList $
                catMaybes $
                  map dcIdentifierEntry $
                    metadataC C.$/ dcIdentifierElement,
            title = listToMaybe titleList',
            description = listToMaybe descriptionList',
            language = listToMaybe languageList',
            creator = listToMaybe creatorList',
            contributors = contributorList',
            publisher = listToMaybe publisherList',
            date = listToMaybe dateList',
            subjects = S.fromList subjectList',
            meta = meta',
            calibreUserCategories = calibreUserCategories',
            coverMediaType = join $ fst <$> mCover,
            coverFileName = snd <$> mCover
          }
  where
    opfPackage = C.element "{http://www.idpf.org/2007/opf}package"
    opfMetadata = C.element "{http://www.idpf.org/2007/opf}metadata"
    opfSchemeAttribute = "{http://www.idpf.org/2007/opf}scheme"
    dcIdentifierElement = C.element "{http://purl.org/dc/elements/1.1/}identifier"
    dcIdentifierEntry c = listToMaybe $ do
      scheme <- c C.$| C.attribute opfSchemeAttribute
      value <- c C.$/ C.content
      return (scheme, value)
    titleList =
      C.element "{http://purl.org/dc/elements/1.1/}title"
        C.&/ C.content
    descriptionList =
      C.element "{http://purl.org/dc/elements/1.1/}description"
        C.&/ C.content
    languageList =
      C.element "{http://purl.org/dc/elements/1.1/}language"
        C.&/ C.content
    creatorList =
      C.element "{http://purl.org/dc/elements/1.1/}creator"
        C.&/ C.content
    contributorList =
      C.element "{http://purl.org/dc/elements/1.1/}contributor"
        C.&/ C.content
    subjectList =
      C.element "{http://purl.org/dc/elements/1.1/}subject"
        C.&/ C.content
    dateList =
      C.element "{http://purl.org/dc/elements/1.1/}date"
        C.&/ C.content
    publisherList =
      C.element "{http://purl.org/dc/elements/1.1/}publisher"
        C.&/ C.content
    metaAlist =
      C.element "{http://www.idpf.org/2007/opf}meta"
        C.&| metaEntry

metaEntry :: C.Cursor -> Maybe (Text, Text)
metaEntry c =
  listToMaybe $
    msum
      [ do
          key <- c C.$| C.attribute "name"
          value <- c C.$| C.attribute "content"
          return (key, value),
        do
          key <- c C.$| C.attribute "property"
          value <- c C.$/ C.content
          return (key, value)
      ]

-- | Return href attribute of the nav item.
--
-- Note that the result will be a relative path from the opf document.
getNavDocumentPathMaybe :: (MonadThrow m) => OpfDocument -> m (Maybe FilePath)
getNavDocumentPathMaybe (OpfDocument root) =
  fmap unpack
    <$> lookupItemAttributeInManifest
      root
      "item[properties=nav]"
      (item C.>=> C.attributeIs "properties" "nav")
      "href"

-- | Return href attribute of ncx.
--
-- Note that the result will be a relative path from the opf document.
getNcxPathMaybe :: (MonadThrow m) => OpfDocument -> m (Maybe FilePath)
getNcxPathMaybe (OpfDocument root) =
  fmap unpack
    <$> lookupItemAttributeInManifest
      root
      "item[media-type=application/x-dtbncx+xml]"
      (item C.>=> C.attributeIs "media-type" "application/x-dtbncx+xml")
      "href"

lookupCover :: (MonadThrow m) => OpfDocument -> m (Maybe (Maybe Text, FilePath))
lookupCover doc = do
  metadata <- getMetadataFromOpf doc
  case coverFileName metadata of
    Nothing -> return Nothing
    Just filepath ->
      return $ Just (coverMediaType metadata, filepath)

lookupCoverById :: (MonadThrow m) => Text -> C.Cursor -> m (Maybe Text, FilePath)
lookupCoverById coverId root = do
  mItem <- lookupElementInManifest root (item >=> C.attributeIs "id" coverId)
  case mItem of
    Nothing -> throwM $ OpfElementNotFound "cover item"
    Just c -> do
      let mediaType = listToMaybe $ c C.$| C.attribute "media-type"
      case c C.$| C.attribute "href" of
        [] -> throwM $ OpfAttributeNotFound "cover" "href"
        (path : _) -> return (mediaType, unpack path)

lookupItemAttributeInManifest ::
  (MonadThrow m) =>
  C.Cursor ->
  String ->
  C.Axis ->
  X.Name ->
  m (Maybe Text)
lookupItemAttributeInManifest root description axis attrName = do
  mCursor <- lookupElementInManifest root axis
  case mCursor of
    Nothing -> return Nothing
    Just c ->
      case C.attribute attrName c of
        [] -> throwM $ OpfAttributeNotFound description attrName
        (value : _) -> return $ Just value

lookupElementInManifest :: (MonadThrow m) => C.Cursor -> C.Axis -> m (Maybe C.Cursor)
lookupElementInManifest root axis =
  case opfManifest root of
    [] -> throwM $ OpfElementNotFound "manifest"
    (manifestCursor : _) ->
      case manifestCursor C.$/ axis of
        [] -> return Nothing
        (c : _) -> return $ Just c

opfManifest :: C.Cursor -> [C.Cursor]
opfManifest =
  C.element "{http://www.idpf.org/2007/opf}package"
    C.&/ C.element "{http://www.idpf.org/2007/opf}manifest"

item :: C.Axis
item = C.element "{http://www.idpf.org/2007/opf}item"

-- Utilities

-- | Extract something from an embedded JSON.
decodeJsonText :: (A.FromJSON a) => Text -> Maybe a
decodeJsonText = A.decodeStrict . T.encodeUtf8
