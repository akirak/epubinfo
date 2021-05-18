-- |
module EPUBInfo.FetchMetadata
  ( mergeOnlineMetadata,
  )
where

import EPUBInfo.Document.Opf

mergeOnlineMetadata :: EPUBMetadata -> IO EPUBMetadata
mergeOnlineMetadata original = do
  let ids = identifierMap original
      mIsbn = M.lookup "ISBN" ids
