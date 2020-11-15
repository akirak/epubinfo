{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import EbookMetadata

prop_test :: Property
prop_test = property $ do
  doEbookMetadata === "EbookMetadata"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
