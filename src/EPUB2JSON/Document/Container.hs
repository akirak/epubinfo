-- |
module EPUB2JSON.Document.Container
  ( Container,
    readContainer,
    getOpfPath,
  )
where

import Control.Monad.Catch (MonadThrow (..))
import Data.Text (unpack)
import EPUB2JSON.Monad
import Protolude
import qualified Text.XML.Cursor as C

newtype Container = Container C.Cursor

readContainer :: EPUBM Container
readContainer = Container . C.fromDocument <$> readXmlInArchive "META-INF/container.xml"

data EPUBNoRootFileError = EPUBNoRootFileError
  deriving (Show, Typeable)

instance Exception EPUBNoRootFileError

getOpfPath :: MonadThrow m => Container -> m FilePath
getOpfPath (Container cursor) =
  case fromCursor cursor of
    [] -> throwM EPUBNoRootFileError
    (a : _) -> return $ unpack a
  where
    -- /container//rootfile@full-path in XPath
    fromCursor =
      C.element "{urn:oasis:names:tc:opendocument:xmlns:container}container"
        C.&// C.element "{urn:oasis:names:tc:opendocument:xmlns:container}rootfile"
        >=> C.attribute "full-path"
