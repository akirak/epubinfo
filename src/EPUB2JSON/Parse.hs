-- |
module EPUB2JSON.Parse
  ( parseEPUBStream,
    EPUBParseException (..),
  )
where

import qualified Codec.Archive.Zip as Z
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import EPUB2JSON.Types
import Protolude
import qualified Text.XML as X
import qualified Text.XML.Cursor as C

data EPUBParseException
  = EPUBZipExtractError Text
  | EPUBXmlParseError FilePath SomeException
  | EPUBXmlDecodeError FilePath UnicodeException
  | EPUBFileNotFoundError FilePath
  | EPUBNoRootFileError
  | EPUBOpfNoMetadata
  deriving (Show, Typeable)

instance Exception EPUBParseException

parseEPUBStream :: LByteString -> Either EPUBParseException EPUB
parseEPUBStream =
  either (Left . EPUBZipExtractError . pack) parseArchive . Z.toArchiveOrFail

parseArchive :: Z.Archive -> Either EPUBParseException EPUB
parseArchive archive = do
  containerDoc <- readXmlFromArchive "META-INF/container.xml" archive
  opfPath <- getOpfPath (C.fromDocument containerDoc)
  opfDoc <- readXmlFromArchive (unpack opfPath) archive
  buildResult $ C.fromDocument opfDoc

readXmlFromArchive :: FilePath -> Z.Archive -> Either EPUBParseException X.Document
readXmlFromArchive relativePath archive =
  case Z.findEntryByPath relativePath archive of
    Nothing -> Left $ EPUBFileNotFoundError relativePath
    Just entry -> parseXml relativePath $ Z.fromEntry entry

parseXml :: FilePath -> LByteString -> Either EPUBParseException X.Document
parseXml relativePath src =
  either (Left . EPUBXmlDecodeError relativePath) Right (LT.decodeUtf8' src)
    >>= either (Left . EPUBXmlParseError relativePath) Right . X.parseText X.def

getOpfPath :: C.Cursor -> Either EPUBParseException Text
getOpfPath =
  maybeToEither EPUBNoRootFileError
    . listToMaybe
    . fromCursor
  where
    -- /container//rootfile@full-path in XPath
    fromCursor =
      C.element "{urn:oasis:names:tc:opendocument:xmlns:container}container"
        C.&// C.element "{urn:oasis:names:tc:opendocument:xmlns:container}rootfile"
        >=> C.attribute "full-path"

buildResult :: C.Cursor -> Either EPUBParseException EPUB
buildResult =
  maybe (Left EPUBOpfNoMetadata) (Right . analyseMetadata)
    . listToMaybe
    . selectMetadata
  where
    selectMetadata =
      C.element "{http://www.idpf.org/2007/opf}package"
        C.&/ C.element "{http://www.idpf.org/2007/opf}metadata"

analyseMetadata :: C.Cursor -> EPUB
analyseMetadata c =
  EPUB
    { identifier = c C.$/ identifierList,
      title = c C.$/ titleList,
      language = c C.$/ languageList,
      creator = c C.$/ creatorList,
      contributor = c C.$/ contributorList,
      publisher = listToMaybe $ c C.$/ maybePublisher,
      date = listToMaybe $ c C.$/ maybeDate,
      subject = c C.$/ subjectList,
      meta = meta,
      calibreUserCategories =
        case M.lookup "calibre:user_categories" meta of
          Just [t] -> decodeText t
          _ -> mempty
    }
  where
    meta = M.fromList $ groupByKey $ catMaybes $ c C.$/ metaAlist
    identifierList =
      C.element "{http://purl.org/dc/elements/1.1/}identifier"
        C.&/ C.content
    titleList =
      C.element "{http://purl.org/dc/elements/1.1/}title"
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
    maybeDate =
      C.element "{http://purl.org/dc/elements/1.1/}date"
        C.&/ C.content
    maybePublisher =
      C.element "{http://purl.org/dc/elements/1.1/}publisher"
        C.&/ C.content
    metaAlist =
      C.element "{http://www.idpf.org/2007/opf}meta"
        C.&| metaFromNode . C.node

groupByKey :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupByKey =
  map (\xs@(x : _) -> (fst x, map snd xs))
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)

metaFromNode :: X.Node -> Maybe (Text, Text)
metaFromNode (X.NodeElement e)
  -- Calibre
  | attrs <- X.elementAttributes e,
    Just name <- M.lookup "name" attrs,
    Just content <- M.lookup "content" (X.elementAttributes e) =
    Just (name, content)
  -- EPUB 3
  | attrs <- X.elementAttributes e,
    Just property <- M.lookup "property" attrs,
    (X.NodeContent content : _) <- X.elementNodes e =
    Just (property, content)
  | otherwise = Nothing

decodeText :: A.FromJSON a => Text -> Maybe a
decodeText = A.decodeStrict . T.encodeUtf8
