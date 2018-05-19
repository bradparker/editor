module Main
  ( main
  ) where

import Control.Monad.State.Lazy (execStateT)
import Data.List (intersperse)
import Lens.Micro.Platform ((%=), (.=))
import System.Environment (getArgs)
import Yi
  ( Action(EditorA, YiA)
  , IndentSettings(..)
  , Mode(..)
  , defaultConfig
  , newTabE
  , onMode
  , openNewFile
  , startEditor
  )
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Simple (lineNumbers, modes, startActions, theme)
import Yi.Config.Simple.Types (ConfigM(runConfigM))

import Editor.Theme (solarizedLight)

configureIndentSettings :: ConfigM ()
configureIndentSettings = modes %= fmap (onMode indentSettings)
  where
    indentSettings mode =
      mode
        { modeIndentSettings =
            (modeIndentSettings mode)
              {expandTabs = True, tabSize = 2, shiftWidth = 2}
        }

main :: IO ()
main = do
  args <- getArgs
  conf <- execStateT (runConfigM (config args)) defaultConfig
  startEditor conf Nothing

config :: [String] -> ConfigM ()
config args = do
  configureVty
  configureVim
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
  configureIndentSettings
  lineNumbers .= True
  theme .= solarizedLight
  startActions .= intersperse (EditorA newTabE) (map (YiA . openNewFile) args)
