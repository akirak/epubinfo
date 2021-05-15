module Main (main) where

import Data.Version (showVersion)
import EPUBInfo
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
  runCmd

-- | Print metadata in JSON
printMetadata :: FilePath -> IO ()
printMetadata file =
  withEPUBFile file getMetadata >>= printJson
