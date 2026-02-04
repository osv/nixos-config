{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
-- |
-- Module      : My.Keybindings
-- Description : XMonad keybindings using structured DSL
--
-- This module defines all XMonad keybindings using a structured DSL that
-- serves as a single source of truth. The same data generates both:
-- 1. XMonad format for 'addDescrKeys''
-- 2. Structured export format for HTML visualization

module My.Keybindings (myKeys, allKeys, allKeysStructured) where

import Data.Char (isSpace, toUpper)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect (bringSelected, goToSelected)
import XMonad.Actions.Promote ( promote )
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.ManageDocks (Direction2D (D, L, R, U), ToggleStruts (ToggleStruts))
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile (MirrorResize (MirrorExpand, MirrorShrink))
import XMonad.Layout.Spacing (decScreenSpacing, decWindowSpacing, incScreenSpacing, incWindowSpacing)
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.MultiToggle as MT
import qualified XMonad.Layout.ToggleLayouts as T
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad ( namedScratchpadAction )
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W

import My.Theme
import My.Grid
import My.ScratchPads
import My.KeybindingTypes (renderKeyName, parseKeyNames, isChord, KeybindingCategory(..), Keybinding(..), KeybindingCategoryDef(..), KeybindingDef(..), category, bind)

myKeys :: XConfig l -> XConfig l
myKeys = addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) allKeys

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe myShowHelpPipedApp
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- | Single source of truth for all keybindings
--
-- This structured data generates both:
-- 1. XMonad keybindings via 'allKeys'
-- 2. HTML export data via 'allKeysStructured'
myKeybindingCategories :: [KeybindingCategoryDef]
myKeybindingCategories =
  [ category "Xmonad Essentials" "xmonad-essentials"
      [ bind "M-C-r"       "Recompile XMonad"                        $ spawn "xmonad --recompile"
      , bind "M-S-r"       "Restart XMonad"                          $ spawn "xmonad --restart"
      , bind "M-S-q"       "Quit XMonad"                             $ sequence_ [spawn (mySoundPlayer ++ shutdownSound), io exitSuccess]
      , bind "M-S-x"       "Kill focused window"                       kill1
      , bind "M-C-x"       "Kill all windows on WS"                    killAll
      , bind "M-S-<F1>"    "Show Help"                               $ spawn "xdg-open ~/.cache/nixos-config/keybinding/index.html"
      , bind "M-p"         "Run prompt"                              $ spawn myLauncher
      ]

  , category "Switch to workspace" "switch-workspace"
      [ bind "M-<Left>"    "Switch to previous workspace"              prevWS
      , bind "M-<Right>"   "Switch to next workspace"                  nextWS
      , bind "M-1"         "Switch to workspace 1"                   $ windows (W.greedyView $ myWorkspaces !! 0)
      , bind "M-2"         "Switch to workspace 2"                   $ windows (W.greedyView $ myWorkspaces !! 1)
      , bind "M-3"         "Switch to workspace 3"                   $ windows (W.greedyView $ myWorkspaces !! 2)
      , bind "M-4"         "Switch to workspace 4"                   $ windows (W.greedyView $ myWorkspaces !! 3)
      , bind "M-5"         "Switch to workspace 5"                   $ windows (W.greedyView $ myWorkspaces !! 4)
      , bind "M-6"         "Switch to workspace 6"                   $ windows (W.greedyView $ myWorkspaces !! 5)
      , bind "M-7"         "Switch to workspace 7"                   $ windows (W.greedyView $ myWorkspaces !! 6)
      , bind "M-8"         "Switch to workspace 8"                   $ windows (W.greedyView $ myWorkspaces !! 7)
      , bind "M-9"         "Switch to workspace 9"                   $ windows (W.greedyView $ myWorkspaces !! 8)
      ]

  , category "Send window to workspace" "send-window"
      [ bind "M-S-1"       "Send to workspace 1"                     $ windows (W.shift $ myWorkspaces !! 0)
      , bind "M-S-2"       "Send to workspace 2"                     $ windows (W.shift $ myWorkspaces !! 1)
      , bind "M-S-3"       "Send to workspace 3"                     $ windows (W.shift $ myWorkspaces !! 2)
      , bind "M-S-4"       "Send to workspace 4"                     $ windows (W.shift $ myWorkspaces !! 3)
      , bind "M-S-5"       "Send to workspace 5"                     $ windows (W.shift $ myWorkspaces !! 4)
      , bind "M-S-6"       "Send to workspace 6"                     $ windows (W.shift $ myWorkspaces !! 5)
      , bind "M-S-7"       "Send to workspace 7"                     $ windows (W.shift $ myWorkspaces !! 6)
      , bind "M-S-8"       "Send to workspace 8"                     $ windows (W.shift $ myWorkspaces !! 7)
      , bind "M-S-9"       "Send to workspace 9"                     $ windows (W.shift $ myWorkspaces !! 8)
      ]

  , category "Move window to WS and go there" "move-window-go"
      [ bind "M-S-<Left>"  "Move window to next WS"                  $ shiftToPrev >> prevWS
      , bind "M-S-<Right>" "Move window to prev WS"                  $ shiftToNext >> nextWS
      ]

  , category "Window navigation" "window-nav"
      [ bind "M-j"         "Move focus to next window"               $ windows W.focusDown
      , bind "M-k"         "Move focus to prev window"               $ windows W.focusUp
      , bind "M-m"         "Move focus to master window"             $ windows W.focusMaster
      , bind "M-S-j"       "Swap focused window with next window"    $ windows W.swapDown
      , bind "M-S-k"       "Swap focused window with prev window"    $ windows W.swapUp
      , bind "M-S-m"       "Swap focused window with master window"  $ windows W.swapMaster
      , bind "M-<Backspace>" "Move focused window to master"           promote
      , bind "M-S-,"       "Rotate all windows except master"          rotSlavesDown
      , bind "M-S-."       "Rotate all windows current stack"          rotAllDown
      ]

  , category "Favorite programs" "favorite-programs"
      [ bind "M-<Space>"   "Terminal"                                $ spawn myTerminal
      , bind "M-f"         "Web browser"                             $ spawn myBrowser
      , bind "M-z"         "Zoom App (boomer)"                       $ spawn "boomer"
      , bind "M-M1-h"      "Launch htop"                             $ spawn (myTerminal ++ " -e htop")
      , bind "M-S-l"       "Lock screen"                             $ spawn "my-screenlock"
      ]

  , category "Monitors" "monitors"
      [ bind "M-."         "Switch focus to next monitor"              nextScreen
      , bind "M-,"         "Switch focus to prev monitor"              prevScreen
      , bind "M-<F2>"      "Display: laptop only"                      $ spawn "autorandr laptop"
      , bind "M-<F3>"      "Display: external only"                    $ spawn "autorandr external"
      , bind "M-<F4>"      "Display: dual monitors"                    $ spawn "autorandr dual"
      ]

  , category "Switch layouts" "switch-layouts"
      [ bind "M-<Return>"  "Switch to next layout"                   $ sendMessage NextLayout
      , bind "M-b"         "Toggle noborders/full"                   $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
      ]

  , category "Window resizing" "window-resizing"
      [ bind "M-["         "Shrink window"                           $ sendMessage Shrink
      , bind "M-]"         "Expand window"                           $ sendMessage Expand
      , bind "M-S-["       "Shrink window vertically"                $ sendMessage MirrorShrink
      , bind "M-S-]"       "Expand window vertically"                $ sendMessage MirrorExpand
      ]

  , category "Floating windows" "floating-windows"
      [ bind "M-C-t"       "Toggle float layout"                     $ sendMessage (T.Toggle "floats")
      , bind "M-t"         "Sink a floating window"                  $ withFocused $ windows . W.sink
      , bind "M-S-t"       "Sink all floated windows"                  sinkAll
      ]

  , category "Window spacing (gaps)" "window-spacing"
      [ bind "M-M1-["      "Decrease window spacing"                 $ decWindowSpacing 2
      , bind "M-M1-]"      "Increase window spacing"                 $ incWindowSpacing 2
      , bind "M-C-["       "Decrease screen spacing"                 $ decScreenSpacing 2
      , bind "M-C-]"       "Increase screen spacing"                 $ incScreenSpacing 2
      ]

  , category "Increase/decrease windows in master pane or the stack" "master-stack"
      [ bind "M-S-<Up>"    "Increase clients in master pane"         $ sendMessage (IncMasterN 1)
      , bind "M-S-<Down>"  "Decrease clients in master pane"         $ sendMessage (IncMasterN (-1))
      , bind "M-="         "Increase max # of windows for layout"      increaseLimit
      , bind "M--"         "Decrease max # of windows for layout"      decreaseLimit
      ]

  , category "Sublayouts" "sublayouts"
      [ bind "M-C-h"       "pullGroup L"                             $ sendMessage $ pullGroup L
      , bind "M-C-l"       "pullGroup R"                             $ sendMessage $ pullGroup R
      , bind "M-C-k"       "pullGroup U"                             $ sendMessage $ pullGroup U
      , bind "M-C-j"       "pullGroup D"                             $ sendMessage $ pullGroup D
      , bind "M-C-m"       "MergeAll"                                $ withFocused (sendMessage . MergeAll)
      , bind "M-C-/"       "UnMergeAll"                              $ withFocused (sendMessage . UnMergeAll)
      , bind "M-C-."       "Switch focus next tab"                   $ onGroup W.focusUp'
      , bind "M-C-,"       "Switch focus prev tab"                   $ onGroup W.focusDown'
      ]

  , category "Scratchpads" "scratchpads"
      [ bind "M-s t"       "Toggle scratchpad terminal"              $ namedScratchpadAction myScratchPads "terminal"
      , bind "M-s m"       "Toggle scratchpad mocp"                  $ namedScratchpadAction myScratchPads "mocp"
      , bind "M-s c"       "Toggle scratchpad calculator"            $ namedScratchpadAction myScratchPads "calculator"
      ]

  , category "Mocp music player" "mocp"
      [ bind "M-u p"       "mocp play"                               $ spawn "mocp --play"
      , bind "M-u l"       "mocp next"                               $ spawn "mocp --next"
      , bind "M-u h"       "mocp prev"                               $ spawn "mocp --previous"
      , bind "M-u <Space>" "mocp toggle pause"                       $ spawn "mocp --toggle-pause"
      ]

  , category "GridSelect" "gridselect"
      [ bind "M-M1-<Return>" "Select favorite apps"                  $ spawnSelectedGrid gsCategories
      , bind "M-w"         "Goto selected window"                    $ goToSelected  $ mygridConfig myGridColorizer
      , bind "M-S-w"       "Bring selected window"                   $ bringSelected $ mygridConfig myGridColorizer
      , bind "M-M1-1"      "1 Menu of games"                         $ spawnSelectedGrid gsGames
      , bind "M-M1-2"      "2 Menu of education apps"                $ spawnSelectedGrid gsEducation
      , bind "M-M1-3"      "3 Menu of Internet apps"                 $ spawnSelectedGrid gsInternet
      , bind "M-M1-4"      "4 Menu of multimedia apps"               $ spawnSelectedGrid gsMultimedia
      , bind "M-M1-5"      "5 Menu of office apps"                   $ spawnSelectedGrid gsOffice
      , bind "M-M1-6"      "6 Menu of settings apps"                 $ spawnSelectedGrid gsSettings
      , bind "M-M1-7"      "7 Menu of system apps"                   $ spawnSelectedGrid gsSystem
      , bind "M-M1-8"      "8 Menu of utilities apps"                $ spawnSelectedGrid gsUtilities
      ]

  , category "Emacs" "emacs"
      [ bind "M-e e"       "Emacsclient Everywhere"                  $ spawn (myEmacs ++ "--eval '(emacs-everywhere)'")
      , bind "M-e c"       "Emacsclient"                             $ spawn myEmacs
      , bind "M-e a"       "Emacsclient EMMS (music)"                $ spawn (myEmacs ++ "--eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/\")'")
      , bind "M-e b"       "Emacsclient Ibuffer"                     $ spawn (myEmacs ++ "--eval '(ibuffer)'")
      , bind "M-e d"       "Emacsclient Dired"                       $ spawn (myEmacs ++ "--eval '(dired nil)'")
      , bind "M-e s"       "Emacsclient Eshell"                      $ spawn (myEmacs ++ "--eval '(eshell)'")
      , bind "M-e v"       "Emacsclient Vterm"                       $ spawn (myEmacs ++ "--eval '(+vterm/here nil)'")
      , bind "M-e w"       "Emacsclient EWW Browser"                 $ spawn (myEmacs ++ "--eval '(doom/window-maximize-buffer(eww \"distro.tube\"))'")
      ]
  ]

-- | Convert to XMonad format (unchanged signature, backward compatible)
--
-- This function converts the structured keybinding definitions into the
-- format expected by XMonad's 'addDescrKeys''. It maintains compatibility
-- with existing code while using the structured data as source of truth.
allKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
allKeys c = concatMap (convertCategory c) myKeybindingCategories
  where
    convertCategory cfg catDef =
      subtitle' (kcdName catDef) :
      mkNamedKeymap cfg [(kdCombo b, addName (kdDesc b) (kdAction b))
                        | b <- kcdBindings catDef]

-- | Convert to structured export format
--
-- This function converts the structured keybinding definitions into the
-- format used for HTML export. No parsing required - direct transformation!
allKeysStructured :: [KeybindingCategory]
allKeysStructured = zipWith assignColor [0..] $ map convertCategory myKeybindingCategories
  where
    convertCategory catDef = KeybindingCategory
      { kcName = kcdName catDef
      , kcId = kcdId catDef
      , kcBindings = map convertBinding (kcdBindings catDef)
      }

    convertBinding b = Keybinding
      { kbCombo = kdCombo b
      , kbKeys = parseKeyNames (kdCombo b)
      , kbAction = kdDesc b
      , kbIsChord = isChord (kdCombo b)
      , kbCatId = Nothing  -- Will be filled during export
      }

    assignColor idx cat = cat { kcBindings = map (assignBindingColor $ kcId cat) (kcBindings cat) }

    assignBindingColor catId kb = kb -- Color is assigned by category, not binding

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                                  $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'
