module Editor.Formatters.RemoveNewlines
  ( removeNewlinesB
  ) where

import Data.Char (isSpace)
import Yi (BufferM, getSelectRegionB, modifyRegionB)
import qualified Yi.Rope as Rope

removeNewlinesB :: BufferM ()
removeNewlinesB = do
  region <- getSelectRegionB
  modifyRegionB removeNewlines region
  where
    removeNewlines = Rope.unwords . map trim . Rope.lines
    trim = Rope.dropWhile isSpace . Rope.dropWhileEnd isSpace
