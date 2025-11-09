-- |

module My.Theme (myWorkspaces
                 , myFont
                 , myTerminal
                 , myLauncher
                 , myModMask
                 , myBrowser
                 , myEmacs
                 , myEditor
                 , myNormColor
                 , myFocusColor
                 , myBorderWidth
                 , myTabTheme
                 , myShowWNameTheme
                 , myShowPipedApp
                 , myShowHelpPipedApp
                 , mySoundPlayer
                 , startupSound
                 , shutdownSound
                 , dmenuSound
                 , myGridColorizer
                 , basebg, basefg
                 , basecr
                 , base00, base08
                 , base01, base09
                 , base02, base10
                 , base03, base11
                 , base04, base12
                 , base05, base13
                 , base06, base14
                 , base07, base15) where

import XMonad
import XMonad.Layout.Tabbed
import XMonad.Layout.ShowWName
import XMonad.Actions.GridSelect
import Colors.DoomOne
import My.Xprop

myWorkspaces :: [String]
myWorkspaces = ["DEV", "WWW", "SYS", "DOC", "VBOX", "CHAT", "MUS", "VID", "GFX"]

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myTerminal :: String
myTerminal = xprop "xmonad.terminal"    -- Sets default terminal

myLauncher  = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myBrowser :: String
myBrowser = "google-chrome-stable "  -- Sets qutebrowser as browser

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor

myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color15     -- This variable is imported from Colors.THEME

-- App for show text via pipe
myShowPipedApp = "zenity --text-info --font=terminus"
myShowHelpPipedApp = "~/.xmonad/bin/dzen2-pager.sh"

mySoundPlayer :: String
mySoundPlayer = "ffplay -nodisp -autoexit " -- The program that will play system sounds

soundDir = "/opt/dtos-sounds/" -- The directory that has the sound files

startupSound  = soundDir ++ "startup-01.mp3"
shutdownSound = soundDir ++ "shutdown-01.mp3"
dmenuSound    = soundDir ++ "menu-01.mp3"

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

-- Theme for tabs layout and tabs sublayout.
myTabTheme:: Theme
myTabTheme = def { fontName            = myFont
                 , activeColor         = color15
                 , inactiveColor       = color08
                 , activeBorderColor   = color15
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Ubuntu:bold:size=40"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#00FA9A"
  , swn_color             = "#1c1f24"
  }

myGridColorizer :: Window -> Bool -> X (String, String)
myGridColorizer = colorRangeFromClassName
                (0x28,0x2c,0x34) -- lowest inactive bg
                (0x28,0x2c,0x34) -- highest inactive bg
                (0xc7,0x92,0xea) -- active bg
                (0xc0,0xa7,0x9a) -- inactive fg
                (0x28,0x2c,0x34) -- active fg

basebg, basefg, basecr, base00, base08, base01, base09, base02, base10, base03, base11, base04, base12, base05, base13, base06, base14, base07, base15 :: String
basebg = xprop "*.background"
basefg = xprop "*.foreground"
basecr = xprop "*.cursorColor"
base00 = xprop "*.color0"
base01 = xprop "*.color1"
base02 = xprop "*.color2"
base03 = xprop "*.color3"
base04 = xprop "*.color4"
base05 = xprop "*.color5"
base06 = xprop "*.color6"
base07 = xprop "*.color7"
base08 = xprop "*.color8"
base09 = xprop "*.color9"
base10 = xprop "*.color10"
base11 = xprop "*.color11"
base12 = xprop "*.color12"
base13 = xprop "*.color13"
base14 = xprop "*.color14"
base15 = xprop "*.color15"
