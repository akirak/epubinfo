{-# LANGUAGE DeriveGeneric #-}

module EPUB2JSON.Document.Opf
  ( -- * Types
    EPUBMetadata (..),

    -- * Document
    OpfDocument,
    readOpfDocument,
    getMetadataFromOpf,
  )
where

import Control.Monad.Catch
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Char as Char
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import EPUB2JSON.Monad
import Protolude
import qualified Text.XML as X
import qualified Text.XML.Cursor as C

data EPUBMetadata = EPUBMetadata
  { identifier :: [Text],
    title :: [Text],
    language :: [Text],
    -- TODO: DCMES optional
    contributor :: [Text],
    creator :: [Text],
    date :: Maybe Text,
    subject :: [Text],
    publisher :: Maybe Text,
    -- type
    -- , dcAttributes :: Map Text Text
    meta :: Map Text [Text],
    -- TODO: Parse links
    -- , links :: Map Text LinkTarget
    -- TODO: Parse the JSON object of calibre:user_categories
    calibreUserCategories :: Maybe (Map Text [Text])
  }
  deriving (Generic)

data LinkTarget = LinkTarget
  { linkMediaType :: Maybe Text,
    linkRefines :: Maybe Text,
    linkProperties :: [Text],
    linkHref :: Text
  }
  deriving (Generic)

instance ToJSON LinkTarget where
  toEncoding =
    A.genericToEncoding $
      A.defaultOptions
        { A.fieldLabelModifier = map Char.toLower . drop 4
        }

instance ToJSON EPUBMetadata where
  toEncoding =
    A.genericToEncoding $
      A.defaultOptions
        { A.omitNothingFields = True
        }

newtype OpfDocument = OpfDocument C.Cursor
  deriving (Show)

readOpfDocument :: FilePath -> EPUBM OpfDocument
readOpfDocument path = OpfDocument . C.fromDocument <$> readXmlInArchive path

data OpfNotFound = OpfNotFound
  deriving (Show, Typeable)

instance Exception OpfNotFound

getMetadataFromOpf :: MonadThrow m => OpfDocument -> m EPUBMetadata
getMetadataFromOpf (OpfDocument cursor) =
  case opfMetadata cursor of
    [] -> throwM OpfNotFound
    (c : _) -> return $ toEPUBMetadata c

opfMetadata :: C.Cursor -> [C.Cursor]
opfMetadata =
  C.element "{http://www.idpf.org/2007/opf}package"
    C.&/ C.element "{http://www.idpf.org/2007/opf}metadata"

toEPUBMetadata :: C.Cursor -> EPUBMetadata
toEPUBMetadata c =
  EPUBMetadata
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
          Just [t] -> decodeJsonText t
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

decodeJsonText :: A.FromJSON a => Text -> Maybe a
decodeJsonText = A.decodeStrict . T.encodeUtf8
