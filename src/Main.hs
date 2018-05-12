module Main
  ( main
  ) where

import Control.Monad.State.Lazy (execStateT)
import Data.List (intersperse)
import Lens.Micro.Platform ((.=))
import System.Environment (getArgs)
import Yi
  ( Action(EditorA, YiA)
  , defaultConfig
  , newTabE
  , openNewFile
  , startEditor
  )
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Simple (lineNumbers, startActions, theme)
import Yi.Config.Simple.Types (ConfigM(runConfigM))

import Editor.Theme (solarizedLight)

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
  configureMiscModes
  lineNumbers .= True
  theme .= solarizedLight
  startActions .= intersperse (EditorA newTabE) (map (YiA . openNewFile) args)
