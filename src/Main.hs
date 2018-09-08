module Main
  ( main
  ) where

import Control.Monad.State.Lazy (execStateT)
import Data.List (intersperse)
import Data.Semigroup ((<>))
import Lens.Micro.Platform ((%=), (.=))
import System.Environment (getArgs)
import Yi
  ( Action(BufferA, EditorA, YiA)
  , IndentSettings(..)
  , Mode(..)
  , defaultConfig
  , deleteTrailingSpaceB
  , newTabE
  , onMode
  , openNewFile
  , preSaveHooks
  , startEditor
  )
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Simple (defaultKm, lineNumbers, modes, startActions, theme)
import Yi.Config.Simple.Types (ConfigM(runConfigM))

import Editor.Theme (solarizedLight)
import Editor.KeymapSet (keymapSet)

configurePresaveHooks :: ConfigM ()
configurePresaveHooks = preSaveHooks %= (<> customHooks)
  where
    customHooks = [BufferA deleteTrailingSpaceB]

configureIndentSettings :: ConfigM ()
configureIndentSettings = modes %= fmap (onMode indentSettings)
  where
    indentSettings mode =
      mode
        { modeIndentSettings =
            IndentSettings {expandTabs = True, tabSize = 2, shiftWidth = 2}
        }

main :: IO ()
main = do
  args <- getArgs
  conf <- execStateT (runConfigM (config args)) defaultConfig
  startEditor conf Nothing

openInTabs :: [String] -> [Action]
openInTabs = intersperse (EditorA newTabE) . map (YiA . openNewFile)

config :: [String] -> ConfigM ()
config args = do
  configureVty
  configureVim
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
  configureIndentSettings
  configurePresaveHooks
  lineNumbers .= True
  theme .= solarizedLight
  startActions .= openInTabs args
  defaultKm .= keymapSet
