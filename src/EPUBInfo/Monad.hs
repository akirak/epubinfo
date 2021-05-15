{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module EPUBInfo.Monad
  ( EPUBM,
    runEPUBM,

    -- * Exception types
    XmlError (..),
    EPUBFileNotFoundError (..),
    EPUBXmlError (..),

    -- * Functions
    readXmlInArchive,
  )
where

import qualified Codec.Archive.Zip as Z
import Control.Monad.Catch
import qualified Control.Monad.Catch as C
import Control.Monad.Catch.Pure (CatchT, runCatchT)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy.Encoding as LT
import Protolude
import qualified Text.XML as X

newtype EPUBM a = EPUBM (CatchT (Reader Z.Archive) a)
  deriving (Functor, Applicative, Monad, MonadThrow)

-- | Do a certain operation on an EPUB archive.
runEPUBM :: MonadThrow m => EPUBM a -> Z.Archive -> m a
runEPUBM (EPUBM m) archive = do
  let r = runReader (runCatchT m) archive
  case r of
    Left e -> throwM e
    Right a -> return a

-- | Error thrown if a specified file does not exist in an EPUB archive.
newtype EPUBFileNotFoundError = EPUBFileNotFoundError FilePath
  deriving (Show, Typeable)

instance Exception EPUBFileNotFoundError

-- | Error during reading an XML stream.
data XmlError
  = XmlDecodeError UnicodeException
  | XmlParseError SomeException
  deriving (Show, Typeable)

instance Exception XmlError

-- | Error during reading an XML from an EPUB archive.
data EPUBXmlError = EPUBXmlError FilePath XmlError
  deriving (Show, Typeable)

instance Exception EPUBXmlError

-- | Read an XML document in an EPUB document.
readXmlInArchive :: FilePath -> EPUBM X.Document
readXmlInArchive relativePath = EPUBM $ do
  archive <- lift ask
  case Z.findEntryByPath relativePath archive of
    Nothing -> throwM $ EPUBFileNotFoundError relativePath
    Just entry -> do
      parseXmlLbs (Z.fromEntry entry)
        `C.catch` (throwM . EPUBXmlError relativePath)

-- | Parse an XML document from a lazy bytestring.
parseXmlLbs :: MonadThrow m => LByteString -> m X.Document
parseXmlLbs = decodeToText >=> parseXmlFromText
  where
    decodeToText lbs =
      case LT.decodeUtf8' (checkBom lbs) of
        Left ue -> throwM $ XmlDecodeError ue
        Right lt -> return (lt :: LText)
    parseXmlFromText lt =
      case X.parseText X.def lt of
        Left e -> throwM $ XmlParseError e
        Right doc -> return doc

-- | If the argument starts with a BOM, strip it.
checkBom :: LByteString -> LByteString
checkBom lbs =
  if LB.isPrefixOf (LB.pack "\xef\xbb\xbf") lbs
    then LB.drop 3 lbs
    else lbs
