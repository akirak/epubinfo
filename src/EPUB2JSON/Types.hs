{-# LANGUAGE DeriveGeneric #-}

-- | Data types for EPUB metadata.
module EPUB2JSON.Types where

import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Char as Char
import Protolude

-- import GHC.Generics

data EPUB = EPUB
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

instance ToJSON EPUB where
  toEncoding =
    A.genericToEncoding $
      A.defaultOptions
        { A.omitNothingFields = True
        }
