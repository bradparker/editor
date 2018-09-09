module Editor.ExCommands
  ( parseText
  , ExCommand
  , EventString
  ) where

import Data.Attoparsec.Text (string)
import Data.Functor (void)
import Data.Text (Text)
import Yi (Action)
import Yi.Keymap.Vim.Common (EventString)
import Yi.Keymap.Vim.Ex (ExCommand, cmdAction, cmdShow)
import Yi.Keymap.Vim.Ex.Commands.Common (impureExCommand, parse)

parseText :: Text -> Action -> EventString -> Maybe ExCommand
parseText txt action =
  parse $ do
    void $ string txt
    pure $ impureExCommand {cmdShow = txt, cmdAction = action}
