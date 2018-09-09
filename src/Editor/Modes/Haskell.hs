{-# LANGUAGE OverloadedStrings #-}

module Editor.Modes.Haskell
  ( haskellMode
  , formatB
  , format
  , exCommands
  ) where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text.Encoding as Text
import Editor.ExCommands (EventString, ExCommand, parseText)
import HIndent (reformat)
import HIndent.Types (defaultConfig)
import Yi
  ( Action(YiA)
  , BufferM
  , Mode
  , YiM
  , getSelectRegionB
  , modifyRegionB
  , withCurrentBuffer
  )
import Yi.Lexer.Haskell (TT)
import Yi.Mode.Haskell (ghciInferType, ghciLoadBuffer, preciseMode)
import Yi.Rope (YiString)
import qualified Yi.Rope as Rope
import Yi.Syntax.Haskell (Tree)

hindent :: YiString -> YiString
hindent doc =
  either
    (const doc)
    (Rope.fromText . Text.decodeUtf8 . toStrict . toLazyByteString)
    (reformat
       defaultConfig
       Nothing
       Nothing
       (Text.encodeUtf8 (Rope.toText doc)))

formatB :: BufferM ()
formatB = do
  region <- getSelectRegionB
  modifyRegionB hindent region

format :: YiM ()
format = withCurrentBuffer formatB

ghciType :: Action
ghciType = YiA $ do
  ghciLoadBuffer
  ghciInferType

exCommands :: [EventString -> Maybe ExCommand]
exCommands =
  [ parseText "GHCIType" ghciType
  ]

haskellMode :: Mode (Tree TT)
haskellMode =  preciseMode
