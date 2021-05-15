module Main (main) where

import qualified Data.Text.IO as T
import Data.Version (showVersion)
import EPUBInfo
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative.Simple
import Paths_epubinfo (version)
import Protolude

fileArgument :: Parser FilePath
fileArgument = strArgument (metavar "FILE")

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
          fileArgument
  setLocaleEncoding utf8
  runCmd

-- | Print metadata in JSON
printMetadata :: FilePath -> IO ()
printMetadata file =
  withEPUBFile file getMetadata >>= printJson

-- | Print the table of contents
printTableOfContents :: FilePath -> IO ()
printTableOfContents file =
  withEPUBFile file getTableOfContents >>= T.putStr . tocToOrg
