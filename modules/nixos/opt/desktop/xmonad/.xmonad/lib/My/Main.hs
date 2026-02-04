 {-# LANGUAGE UnicodeSyntax #-}

module My.Main (mainXmonad) where

-- Base
import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout.ShowWName

import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.SpawnOnce

import Colors.DoomOne
import My.Layout
import My.Keybindings
import My.KeybindingsExport
import My.ScratchPads
import My.Theme
import My.Xprop
import My.ManageHook
import My.ServerEventHook

import XMonad.Hooks.PrettyStatusBar (PSBConfig(psbOutput, psbIcons))

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "my-set-wallpaper"
  spawn "killall polybar 2>/dev/null; sleep 1 && polybar -c ~/.config/xmonad-polybar/temp-polybar.ini &"
  spawnOnce "xsetroot -cursor_name left_ptr" -- I don't like default "X" cursor over desktop
  setWMName "LG3D"
  exportKeybindings  -- Export keybindings to HTML visualization

myLayoutIcons :: M.Map String String
myLayoutIcons =
  M.fromList
    [
      ("tall",    "█ █")
    , ("monocle", "███")
    ]

mainXmonad :: IO ()
mainXmonad = do
  xmonad $ myKeys $ ewmh $ docks $ def
    { manageHook         = myManageHook <+> manageDocks
    , handleEventHook    =  myServerModeEventHook
                            <+>  windowedFullscreenFixEventHook <> swallowEventHook (-- className =? "Alacritty"  <||>
                                                                                     className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook
    , modMask            = myModMask
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , layoutHook         = showWName' myShowWNameTheme myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    -- , logHook = dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ myLogHook
    -- logHook = clickablePP xmobarPP { ... } >>= dynamicLogWithPP

    , logHook = myPolybarHooks -- dynamicLogWithPP <=< dynamicIconsPP myIconConfig $ filterOutWsPP [scratchpadWorkspaceTag] $ myLogHook
    , focusFollowsMouse = False
    }

myPolybarHooks :: X ()
myPolybarHooks = layoutHookForPolybar
  where
    layoutHookForPolybar = do
      layout <- description . W.layout . W.workspace . W.current . windowset <$> get
      io $ appendFile "/tmp/.xmonad-layout-log"  (renamedLayout layout ++ "\n")
        where renamedLayout n = M.findWithDefault n n myLayoutIcons
