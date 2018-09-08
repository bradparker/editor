{-# LANGUAGE OverloadedStrings #-}

module Editor.KeymapSet
  ( keymapSet
  ) where

import Editor.Formatters.Haskell (haskellFormatB)
import Editor.Formatters.RemoveNewlines (removeNewlinesB)
import Yi
  ( EditorM
  , Event
  , KeymapSet
  , YiM
  , char
  , ctrl
  , override
  , shift
  , withCurrentBuffer
  )
import Yi.Fuzzy (fuzzyOpen)
import Yi.Keymap.Vim
  ( VimBinding
  , defVimConfig
  , mkKeymapSet
  , pureEval
  , vimBindings
  )
import Yi.Keymap.Vim.Common (RepeatToken(Drop), VimMode(Normal, Visual))
import Yi.Keymap.Vim.Utils (mkBindingE, mkBindingY)
import Yi.Types (RegionStyle(Inclusive))

nmap :: Event -> EditorM () -> VimBinding
nmap event editor = mkBindingE Normal Drop (event, editor, id)

vlmap :: Event -> EditorM () -> VimBinding
vlmap event editor = mkBindingE (Visual Inclusive) Drop (event, editor, id)

nmapY :: Event -> YiM () -> VimBinding
nmapY event editor = mkBindingY Normal (event, editor, id)

keymapSet :: KeymapSet
keymapSet =
  mkKeymapSet $
  defVimConfig `override` \super this ->
    super
      { vimBindings =
          vimBindings super ++
          [ nmap (ctrl (char 'l')) (pureEval this ":nohlsearch<CR>")
          , nmapY (ctrl (char 'p')) fuzzyOpen
          , vlmap (char '=') (withCurrentBuffer haskellFormatB)
          , vlmap (shift (char 'j')) (withCurrentBuffer removeNewlinesB)
          ]
      }
