module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBC8
import Data.Version (showVersion)
import EPUB2JSON
import Options.Applicative.Simple
import Paths_epubinfo (version)
import Protolude

main :: IO ()
main = do
  (file, ()) <-
    simpleOptions
      ("epubinfo " ++ showVersion version)
      "epubinfo"
      "Extract metadata from EPUB"
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
