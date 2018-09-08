module Editor.Formatters.Haskell
  ( haskellFormatB
  ) where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text.Encoding as Text
import HIndent (reformat)
import HIndent.Types (defaultConfig)
import Yi (BufferM, getSelectRegionB, modifyRegionB)
import qualified Yi.Rope as Rope

haskellFormatB :: BufferM ()
haskellFormatB = do
  region <- getSelectRegionB
  modifyRegionB hindentFormat region
  where
    hindentFormat doc =
      either
        (const doc)
        (Rope.fromText . Text.decodeUtf8 . toStrict . toLazyByteString)
        (reformat
           defaultConfig
           Nothing
           Nothing
           (Text.encodeUtf8 (Rope.toText doc)))
