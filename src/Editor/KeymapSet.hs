{-# LANGUAGE OverloadedStrings #-}

module Editor.KeymapSet
  ( keymapSet
  ) where

import Data.Semigroup ((<>))
import Editor.Formatters.RemoveNewlines (removeNewlinesB)
import qualified Editor.Modes.Haskell as Haskell
import Yi
  ( EditorM
  , Event
  , KeymapSet
  , Mode(modeName)
  , YiM
  , char
  , ctrl
  , override
  , shift
  , withCurrentBuffer
  , withModeY
  )
import Yi.Fuzzy (fuzzyOpen)
import Yi.Keymap.Vim
  ( VimBinding
  , defVimConfig
  , mkKeymapSet
  , vimBindings
  , vimExCommandParsers
  )
import Yi.Keymap.Vim.Common (RepeatToken(Drop), VimMode(Normal, Visual))
import Yi.Keymap.Vim.Utils (mkBindingE, mkBindingY)
import Yi.Search (resetRegexE)
import Yi.Types (RegionStyle(Inclusive))

nmap :: Event -> EditorM () -> VimBinding
nmap event editor = mkBindingE Normal Drop (event, editor, id)

vlmap :: Event -> EditorM () -> VimBinding
vlmap event editor = mkBindingE (Visual Inclusive) Drop (event, editor, id)

nmapY :: Event -> YiM () -> VimBinding
nmapY event editor = mkBindingY Normal (event, editor, id)

vlmapY :: Event -> YiM () -> VimBinding
vlmapY event editor = mkBindingY (Visual Inclusive) (event, editor, id)

format :: Mode syntax -> YiM ()
format mode
  | modeName mode == "precise haskell" = Haskell.format
  | otherwise = pure ()

keymapSet :: KeymapSet
keymapSet =
  mkKeymapSet $
  defVimConfig `override` \super _ ->
    super
      { vimBindings =
          vimBindings super <>
          [ nmap (ctrl (char 'l')) resetRegexE
          , nmapY (ctrl (char 'p')) fuzzyOpen
          , vlmap (shift (char 'j')) (withCurrentBuffer removeNewlinesB)
          , vlmapY (char '=') (withModeY format)
          ]
      , vimExCommandParsers =
          vimExCommandParsers super <>
          Haskell.exCommands
      }
