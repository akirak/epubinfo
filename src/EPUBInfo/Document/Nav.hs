{-# LANGUAGE OverloadedStrings #-}

-- |
module EPUBInfo.Document.Nav
  ( -- * Nav document
    NavDocument,
    readNavDocument,
  )
where

import Control.Monad.Catch (MonadThrow (..))
import EPUBInfo.Monad
import EPUBInfo.Toc
import Protolude
import qualified Text.XML.Cursor as C
import Prelude (Show (..), String)

newtype NavDocument = NavDocument C.Cursor
  deriving (Show)

readNavDocument :: FilePath -> EPUBM NavDocument
readNavDocument path = NavDocument . C.fromDocument <$> readXmlInArchive path

data NavException
  = NavMissing
  | NavEmpty
  | NavMatchError String
  | NavOther String
  deriving (Typeable)

instance Show NavException where
  show NavMissing = "nav element is missing"
  show NavEmpty = "nav element contains no child"
  show (NavMatchError msg) = "Pattern matching error: " ++ msg
  show (NavOther msg) = "Error in the nav document: " ++ msg

instance Exception NavException

instance ToTableOfContents NavDocument where
  toTableOfContents (NavDocument root) =
    case root C.$// navElement of
      [] -> throwM NavMissing
      (c : _) ->
        case c C.$/ C.anyElement of
          [] -> throwM NavEmpty
          [ol] -> TableOfContents <$> xmlToTocNodes ol
          _ -> throwM $ NavOther "The nav element contains multiple children"

navElement :: C.Axis
navElement =
  C.element "{http://www.w3.org/1999/xhtml}nav"
    C.>=> C.attributeIs "{http://www.idpf.org/2007/ops}type" "toc"

xmlToTocNodes :: MonadThrow m => C.Cursor -> m [TocNode]
xmlToTocNodes = goList
  where
    goList listC =
      mapM goItem $ listC C.$/ C.element "{http://www.w3.org/1999/xhtml}li"
    goItem itemC =
      TocNode
        <$> ( case itemC C.$/ C.element "{http://www.w3.org/1999/xhtml}a" of
                [] -> throwM $ NavMatchError "no anchor"
                [a] -> goEntry a
                _ -> throwM $ NavMatchError "more than one anchors"
            )
        <*> ( case itemC C.$/ C.element "{http://www.w3.org/1999/xhtml}ol" of
                [] -> return []
                [ol] -> goList ol
                _ -> throwM $ NavMatchError "mroe than one lists"
            )
    goEntry anchorC = do
      let contents = anchorC C.$/ C.content
          hrefs = anchorC C.$| C.attribute "href"
      content <- case contents of
        [] -> throwM $ NavOther "anchor contains no text"
        (content : _) -> return content
      href <- case hrefs of
        [] -> throwM $ NavOther "anchor has no href attribute"
        (href : _) -> return href
      return
        TocEntry
          { tocEntryTitle = content,
            tocEntryHref = href
          }
