module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBC8
import Data.Version (showVersion)
import EPUB2JSON
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

printMetadata :: FilePath -> IO ()
printMetadata file = do
  metadata <-
    withEPUBFile file $
      do
        readContainer
        >>= getOpfPath
        >>= readOpfDocument
        >>= getMetadataFromOpf
  LBC8.putStrLn $ A.encode metadata
