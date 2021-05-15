module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBC8
import Data.Version (showVersion)
import EPUB2JSON
import Options.Applicative.Simple
import Paths_epub2json (version)
import Protolude

main :: IO ()
main = do
  (file, ()) <-
    simpleOptions
      ("epub2json " ++ showVersion version)
      "epub2json"
      "Convert EPUB metadata to JSON"
      (strArgument (metavar "FILE"))
      empty
  metadata <-
    withEPUBFile file $
      do
        readContainer
        >>= getOpfPath
        >>= readOpfDocument
        >>= getMetadataFromOpf
  LBC8.putStrLn $ A.encode metadata
