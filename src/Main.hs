{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.Text.IO as T
import Data.Version (showVersion)
import EPUBInfo
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative.Simple
import Paths_epubinfo (version)
import Protolude hiding (option)

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
