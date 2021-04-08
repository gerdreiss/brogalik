module UI.TUI.GameWidgets where


import           Brick                   hiding ( Size )
import           Brick.Widgets.Border           ( border )
import           Brick.Widgets.Border.Style     ( unicodeBold )
import           Brick.Widgets.Center           ( hCenter
                                                , vCenter
                                                )
import           Control.Display                ( renderBrogalik )
import           Data.Brogalik                  ( Brogalik(..)
                                                , Player(..)
                                                )
import           Data.Geom                      ( Height
                                                , Size(..)
                                                , Width
                                                )


-- | The game field (center right)
game :: Brogalik -> Size -> Widget ()
game brogalik size = str $ renderBrogalik brogalik size
