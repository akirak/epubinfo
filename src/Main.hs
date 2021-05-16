{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad.Catch (MonadThrow (..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import EPUBInfo
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative.Simple
import Paths_epubinfo (version)
import Protolude hiding (option)
import System.Directory (doesFileExist)
import qualified System.FilePath as P
import System.IO.Error

fileArgument :: Parser FilePath
fileArgument = strArgument (metavar "FILE")

data TocFormat = TocMarkdown | TocOrg
  deriving (Show)

data TocArguments = TocArguments
  { tocFormat :: TocFormat,
    tocRenderOptions :: TocRenderOptions,
    tocFile :: FilePath
  }
  deriving (Show)

pTocArguments =
  TocArguments
    <$> pTocFormat
    <*> pTocRenderOptions
    <*> fileArgument
  where
    pTocFormat =
      flag'
        TocMarkdown
        ( long "markdown"
            <> help "Markdown output"
        )
        <|> flag'
          TocOrg
          ( long "org"
              <> help "Org output"
          )

    pTocRenderOptions =
      TocRenderOptions
        <$> flag
          False
          True
          ( long "checkbox"
              <> help "Add a checkbox to each item"
          )
        <*> optional
          ( option
              auto
              ( long "depth"
                  <> metavar "INT"
                  <> help "Maximum depth of items to render"
              )
          )

data CoverArguments = CoverArguments
  { coverOutputFile :: Maybe FilePath,
    coverForce :: Bool,
    coverInputFile :: FilePath
  }
  deriving (Show)

pCoverArguments =
  CoverArguments
    <$> optional
      ( option
          str
          ( long "output-file" <> short 'o'
              <> metavar "FILE"
              <> help "Output file"
          )
      )
    <*> flag
      False
      True
      ( long "force" <> short 'f'
          <> help "Overwrite the output file if it exists"
      )
    <*> fileArgument

newtype UnknownMimeExt = UnknownMimeExt Text
  deriving (Show, Typeable)

instance Exception UnknownMimeExt

saveCoverImage :: CoverArguments -> IO ()
saveCoverImage CoverArguments {..} = do
  -- Returning a tuple containing a huge lbs can be bad in terms of
  -- memory footprint. Maybe I should add MonadIO support to EPUBM monad.
  (defaultExt, lbs) <- withEPUBFile coverInputFile $ do
    (mMime, coverPath) <- getCoverImage
    defaultExt <- case mMime of
      Just "image/jpeg" -> return ".jpg"
      Just "image/png" -> return ".png"
      Just mime -> throwM $ UnknownMimeExt mime
      Nothing -> return $ P.takeExtension coverPath
    (defaultExt,) <$> getArchiveContent coverPath
  let outputFile = case coverOutputFile of
        Just oPath -> oPath
        Nothing -> P.replaceExtension coverInputFile defaultExt
  outputExists <- doesFileExist outputFile
  when outputExists $ do
    hPutStrLn stderr $ "File " ++ outputFile ++ " already exists"
    unless coverForce $
      ioError $
        mkIOError
          alreadyExistsErrorType
          "Output file"
          Nothing
          (Just outputFile)
  LB.writeFile outputFile lbs
  putStrLn outputFile

main :: IO ()
main = do
  (_, runCmd) <-
    simpleOptions
      ("epubinfo " ++ showVersion version)
      "epubinfo"
      "Extract metadata from EPUB"
      (pure ())
      $ do
        addCommand
          "metadata"
          "Show metadata of a file"
          printMetadata
          fileArgument
        addCommand
          "toc"
          "Print the table of contents"
          printTableOfContents
          pTocArguments
        addCommand
          "cover"
          "Save the cover image"
          saveCoverImage
          pCoverArguments
  setLocaleEncoding utf8
  runCmd

-- | Print metadata in JSON
printMetadata :: FilePath -> IO ()
printMetadata file =
  withEPUBFile file getMetadata >>= printJson

-- | Print the table of contents
printTableOfContents :: TocArguments -> IO ()
printTableOfContents TocArguments {..} =
  withEPUBFile tocFile getTableOfContents
    >>= T.putStr . renderer tocRenderOptions
  where
    renderer = case tocFormat of
      TocMarkdown -> tocToMarkdown
      TocOrg -> tocToOrg
