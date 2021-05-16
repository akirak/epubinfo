-- | Data types for the table of contents of a document

module EPUBInfo.Toc
  ( -- * Data types
    TableOfContents(..),
    TocNode(..),
    TocEntry(..),
    -- * Type classes
    ToTableOfContents(..),
    -- * Rendering
    TocRenderOptions(..),
    tocToOrg,
    tocToMarkdown,
  )
  where

import Control.Monad.Catch (MonadThrow (..))
import qualified Data.Text as T
import Protolude

newtype TableOfContents = TableOfContents [TocNode]
  deriving (Show)

data TocNode = TocNode
  { tocNodeEntry :: TocEntry,
    tocNodeChildren :: [TocNode]
  }
  deriving (Show)

data TocEntry = TocEntry
  { tocEntryTitle :: Text,
    tocEntryHref :: Text
  }
  deriving (Show)

class ToTableOfContents a where
  toTableOfContents :: MonadThrow m => a -> m TableOfContents

data TocRenderOptions = TocRenderOptions
  { tocCheckbox :: Bool,
    tocMaxDepth :: Maybe Int
  }
  deriving (Show)

tocToOrg :: TocRenderOptions -> TableOfContents -> Text
tocToOrg opts (TableOfContents nodes) = tocToPlainText "-" 2 opts nodes

tocToMarkdown :: TocRenderOptions -> TableOfContents -> Text
tocToMarkdown opts (TableOfContents nodes) = tocToPlainText "-" 2 opts nodes

tocToPlainText :: Text -> Int -> TocRenderOptions -> [TocNode] -> Text
tocToPlainText bullet indentOffset opts = T.intercalate "\n" . map (go 0)
  where
    go level (TocNode entry children) =
      T.concat $
        [ T.replicate (indentOffset * level) (T.singleton ' '),
          bullet,
          if tocCheckbox opts then " [ ]" else T.empty,
          T.singleton ' ',
          tocEntryTitle entry
        ]
          ++ case tocMaxDepth opts of
            Just maxDepth | maxDepth == level + 1 -> []
            _ -> map (T.cons '\n' . go (level + 1)) children
