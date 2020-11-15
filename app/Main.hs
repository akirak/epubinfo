module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBC8
import EbookMetadata.EPUB
import Protolude
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    -- TODO
    [] -> return ()
    (file : _) -> do
      metadata <- readEPUBFile file
      LBC8.putStrLn $ A.encode metadata
