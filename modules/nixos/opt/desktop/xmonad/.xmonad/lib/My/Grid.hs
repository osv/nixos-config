module My.Grid
  ( mygridConfig,
    spawnSelectedGrid,
    gsCategories,
    gsGames,
    gsEducation,
    gsInternet,
    gsMultimedia,
    gsOffice,
    gsSettings,
    gsSystem,
    gsUtilities,
  )
where

import qualified Data.Map as M
import XMonad
import XMonad.Actions.GridSelect

import My.Theme

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move   (-1,  0) >> myNavigation)
         ,((0,xK_h)     , move   (-1,  0) >> myNavigation)
         ,((0,xK_Right) , move   ( 1,  0) >> myNavigation)
         ,((0,xK_l)     , move   ( 1,  0) >> myNavigation)
         ,((0,xK_Down)  , move   ( 0,  1) >> myNavigation)
         ,((0,xK_j)     , move   ( 0,  1) >> myNavigation)
         ,((0,xK_Up)    , move   ( 0, -1) >> myNavigation)
         ,((0,xK_k)     , move   ( 0, -1) >> myNavigation)
         ,((0,xK_y)     , move   (-1, -1) >> myNavigation)
         ,((0,xK_i)     , move   ( 1, -1) >> myNavigation)
         ,((0,xK_n)     , move   (-1,  1) >> myNavigation)
         ,((0,xK_m)     , move   ( 1, -1) >> myNavigation)
         ,((0,xK_space) , setPos ( 0,  0) >> myNavigation)
         ]
       navDefaultHandler = const myNavigation

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myGridColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_navigate     = myNavigation
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelectedGrid :: [(String, String)] -> X ()
spawnSelectedGrid lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 180
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
    selectedActionM <- gridselect conf actions
    case selectedActionM of
        Just selectedAction -> selectedAction
        Nothing -> return ()

gsCategories' =
  [ ("Games",      spawnSelectedGrid gsGames)
  --, ("Education",   spawnSelected' gsEducation)
  , ("Internet",   spawnSelectedGrid gsInternet)
  , ("Multimedia", spawnSelectedGrid gsMultimedia)
  , ("Office",     spawnSelectedGrid gsOffice)
  , ("Settings",   spawnSelectedGrid gsSettings)
  , ("System",     spawnSelectedGrid gsSystem)
  , ("Utilities",  spawnSelectedGrid gsUtilities)
  ]

gsCategories =
  [ ("Games",      "xdotool key super+alt+1")
  , ("Education",  "xdotool key super+alt+2")
  , ("Internet",   "xdotool key super+alt+3")
  , ("Multimedia", "xdotool key super+alt+4")
  , ("Office",     "xdotool key super+alt+5")
  , ("Settings",   "xdotool key super+alt+6")
  , ("System",     "xdotool key super+alt+7")
  , ("Utilities",  "xdotool key super+alt+8")
  ]

gsGames =
  [ ("0 A.D.", "0ad")
  , ("Battle For Wesnoth", "wesnoth")
  , ("OpenArena", "openarena")
  , ("Sauerbraten", "sauerbraten")
  , ("Steam", "steam")
  , ("Unvanquished", "unvanquished")
  , ("Xonotic", "xonotic-glx")
  ]

gsEducation =
  [ ("GCompris", "gcompris-qt")
  , ("Kstars", "kstars")
  , ("Minuet", "minuet")
  , ("Scratch", "scratch")
  ]

gsInternet =
  [ ("Brave", "brave")
  , ("Discord", "discord")
  , ("Element", "element-desktop")
  , ("Firefox", "firefox")
  , ("Chromium", "chromium-browser")
  , ("Google Chrome Stable", "google-chrome-stable")
  , ("Transmission", "transmission-gtk")
  , ("Zoom", "zoom")
  ]

gsMultimedia =
  [ ("Audacity", "audacity")
  , ("Blender", "blender")
  , ("Deadbeef", "deadbeef")
  , ("Kdenlive", "kdenlive")
  , ("OBS Studio", "obs")
  , ("VLC", "vlc")
  ]

gsOffice =
  [ ("Document Viewer", "evince")
  , ("LibreOffice", "libreoffice")
  , ("LO Base", "lobase")
  , ("LO Calc", "localc")
  , ("LO Draw", "lodraw")
  , ("LO Impress", "loimpress")
  , ("LO Math", "lomath")
  , ("LO Writer", "lowriter")
  ]

gsSettings =
  [ ("ARandR (Monitor position)", "arandr")
  ]

gsSystem =
  [ ("Alacritty", "alacritty")
  , ("Xterm", "xterm")
  , ("Bash", myTerminal ++ " -e bash")
  , ("Zsh", myTerminal ++ " -e zsh")
  , ("Fish", myTerminal ++ " -e fish")
  , ("Htop", myTerminal ++ " -e htop")
  , ("PCManFM", "pcmanfm")
  , ("VirtualBox", "virtualbox")
  , ("Virt-Manager", "virt-manager")
  , ("Zsh", myTerminal ++ " -e zsh")
  ]

gsUtilities =
  [ ("Emacs", "emacs")
  , ("Emacsclient", "emacsclient -c -a 'emacs'")
  , ("Nitrogen", "nitrogen")
  , ("Vim", myTerminal ++ " -e vim")
  ]
