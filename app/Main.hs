module Main where

import qualified System.Console.Terminal.Size  as Terminal

import           System.Console.Terminal.Size   ( Window(height, width) )
import           System.Exit                    ( exitFailure )
import           UI.TUI.Main                    ( tui )

main :: IO ()
main = Terminal.size >>= maybe exitFailure renderTui

renderTui :: Window Int -> IO ()
renderTui window = tui (width window) (height window)

