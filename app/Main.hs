module Main where

import           System.Console.Terminal.Size
import           System.Exit
import           UI.TUI.Main                    ( tui )

main :: IO ()
main = size >>= maybe exitFailure renderTui

renderTui :: Window Int -> IO ()
renderTui window = tui (width window) (height window)

