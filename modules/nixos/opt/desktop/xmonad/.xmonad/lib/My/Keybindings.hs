{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
-- |

module My.Keybindings (myKeys) where

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

myKeys :: XConfig l -> XConfig l
myKeys = addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) allKeys

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe myShowHelpPipedApp
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

allKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
allKeys c =
  --(subtitle "Custom Keys":) $ mkNamedKeymap c $
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
  subKeys "Xmonad Essentials"
  [ ("M-C-r",       cmd "Recompile XMonad"                        $ spawn "xmonad --recompile")
  , ("M-S-r",       cmd "Restart XMonad"                          $ spawn "xmonad --restart")
  , ("M-S-q",       cmd "Quit XMonad"                             $ sequence_ [spawn (mySoundPlayer ++ shutdownSound), io exitSuccess])
  , ("M-S-x",       cmd "Kill focused window"                       kill1)
  , ("M-C-x",       cmd "Kill all windows on WS"                    killAll)
  , ("M-p",         cmd "Run prompt"                              $ spawn myLauncher)]

  ^++^ subKeys "Switch to workspace"
  [ ("M-<Left>",    cmd "Switch to previous workspace"              prevWS)
  , ("M-<Right>",   cmd "Switch to next workspace"                  nextWS)
  , ("M-1",         cmd "Switch to workspace 1"                   $ windows (W.greedyView $ myWorkspaces !! 0))
  , ("M-2",         cmd "Switch to workspace 2"                   $ windows (W.greedyView $ myWorkspaces !! 1))
  , ("M-3",         cmd "Switch to workspace 3"                   $ windows (W.greedyView $ myWorkspaces !! 2))
  , ("M-4",         cmd "Switch to workspace 4"                   $ windows (W.greedyView $ myWorkspaces !! 3))
  , ("M-5",         cmd "Switch to workspace 5"                   $ windows (W.greedyView $ myWorkspaces !! 4))
  , ("M-6",         cmd "Switch to workspace 6"                   $ windows (W.greedyView $ myWorkspaces !! 5))
  , ("M-7",         cmd "Switch to workspace 7"                   $ windows (W.greedyView $ myWorkspaces !! 6))
  , ("M-8",         cmd "Switch to workspace 8"                   $ windows (W.greedyView $ myWorkspaces !! 7))
  , ("M-9",         cmd "Switch to workspace 9"                   $ windows (W.greedyView $ myWorkspaces !! 8))]

  ^++^ subKeys "Send window to workspace"
  [ ("M-S-1",       cmd "Send to workspace 1"                     $ windows (W.shift $ myWorkspaces !! 0))
  , ("M-S-2",       cmd "Send to workspace 2"                     $ windows (W.shift $ myWorkspaces !! 1))
  , ("M-S-3",       cmd "Send to workspace 3"                     $ windows (W.shift $ myWorkspaces !! 2))
  , ("M-S-4",       cmd "Send to workspace 4"                     $ windows (W.shift $ myWorkspaces !! 3))
  , ("M-S-5",       cmd "Send to workspace 5"                     $ windows (W.shift $ myWorkspaces !! 4))
  , ("M-S-6",       cmd "Send to workspace 6"                     $ windows (W.shift $ myWorkspaces !! 5))
  , ("M-S-7",       cmd "Send to workspace 7"                     $ windows (W.shift $ myWorkspaces !! 6))
  , ("M-S-8",       cmd "Send to workspace 8"                     $ windows (W.shift $ myWorkspaces !! 7))
  , ("M-S-9",       cmd "Send to workspace 9"                     $ windows (W.shift $ myWorkspaces !! 8))]

  ^++^ subKeys "Move window to WS and go there"
  [ ("M-S-<Left>",  cmd "Move window to next WS"                  $ shiftToPrev >> prevWS)
  , ("M-S-<Right>", cmd "Move window to prev WS"                  $ shiftToNext >> nextWS)]

  ^++^ subKeys "Window navigation"
  [ ("M-j",         cmd "Move focus to next window"               $ windows W.focusDown)
  , ("M-k",         cmd "Move focus to prev window"               $ windows W.focusUp)
  , ("M-m",         cmd "Move focus to master window"             $ windows W.focusMaster)
  , ("M-S-j",       cmd "Swap focused window with next window"    $ windows W.swapDown)
  , ("M-S-k",       cmd "Swap focused window with prev window"    $ windows W.swapUp)
  , ("M-S-m",       cmd "Swap focused window with master window"  $ windows W.swapMaster)
  , ("M-<Backspace>", cmd "Move focused window to master"           promote)
  , ("M-S-,",       cmd "Rotate all windows except master"          rotSlavesDown)
  , ("M-S-.",       cmd "Rotate all windows current stack"          rotAllDown)]

  ^++^ subKeys "Favorite programs"
  [ ("M-<Space>",   cmd "Terminal"                                $ spawn myTerminal)
  , ("M-f",         cmd "Web browser"                             $ spawn myBrowser)
  , ("M-z",         cmd "Zoom App (boomer)"                       $ spawn "boomer")
  , ("M-M1-h",      cmd "Launch htop"                             $ spawn (myTerminal ++ " -e htop"))
  , ("M-l",         cmd "Lock screen"                             $ spawn "my-screenlock")]

  ^++^ subKeys "Monitors"
  [ ("M-.",         cmd "Switch focus to next monitor"              nextScreen)
  , ("M-,",         cmd "Switch focus to prev monitor"              prevScreen)]

  -- Switch layouts
  ^++^ subKeys "Switch layouts"
  [ ("M-<Return>",  cmd "Switch to next layout"                   $ sendMessage NextLayout)
  , ("M-b",         cmd "Toggle noborders/full"                   $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)]

  -- Window resizing
  ^++^ subKeys "Window resizing"
  [ ("M-[",         cmd "Shrink window"                           $ sendMessage Shrink)
  , ("M-]",         cmd "Expand window"                           $ sendMessage Expand)
  , ("M-S-[",       cmd "Shrink window vertically"                $ sendMessage MirrorShrink)
  , ("M-S-]",       cmd "Expand window vertically"                $ sendMessage MirrorExpand)]

  -- Floating windows
  ^++^ subKeys "Floating windows"
  [ ("M-C-t",       cmd "Toggle float layout"                     $ sendMessage (T.Toggle "floats"))
  , ("M-t",         cmd "Sink a floating window"                  $ withFocused $ windows . W.sink)
  , ("M-S-t",       cmd "Sink all floated windows"                  sinkAll)]

  -- Increase/decrease spacing (gaps)
  ^++^ subKeys "Window spacing (gaps)"
  [ ("M-M1-[",      cmd "Decrease window spacing"                 $ decWindowSpacing 2)
  , ("M-M1-]",      cmd "Increase window spacing"                 $ incWindowSpacing 2)
  , ("M-C-[",       cmd "Decrease screen spacing"                 $ decScreenSpacing 2)
  , ("M-C-]",       cmd "Increase screen spacing"                 $ incScreenSpacing 2)]

  -- Increase/decrease windows in the master pane or the stack
  ^++^ subKeys "Increase/decrease windows in master pane or the stack"
  [ ("M-S-<Up>",    cmd "Increase clients in master pane"         $ sendMessage (IncMasterN 1))
  , ("M-S-<Down>",  cmd "Decrease clients in master pane"         $ sendMessage (IncMasterN (-1)))
  , ("M-=",         cmd "Increase max # of windows for layout"      increaseLimit)
  , ("M--",         cmd "Decrease max # of windows for layout"      decreaseLimit)]

  -- Sublayouts
  -- This is used to push windows to tabbed sublayouts, or pull them out of it.
  ^++^ subKeys "Sublayouts"
  [ ("M-C-h",       cmd "pullGroup L"                             $ sendMessage $ pullGroup L)
  , ("M-C-l",       cmd "pullGroup R"                             $ sendMessage $ pullGroup R)
  , ("M-C-k",       cmd "pullGroup U"                             $ sendMessage $ pullGroup U)
  , ("M-C-j",       cmd "pullGroup D"                             $ sendMessage $ pullGroup D)
  , ("M-C-m",       cmd "MergeAll"                                $ withFocused (sendMessage . MergeAll))
  -- , ("M-C-u", cmd "UnMerge"               $ withFocused (sendMessage . UnMerge))
  , ("M-C-/",       cmd "UnMergeAll"                              $ withFocused (sendMessage . UnMergeAll))
  , ("M-C-.",       cmd "Switch focus next tab"                   $ onGroup W.focusUp')
  , ("M-C-,",       cmd "Switch focus prev tab"                   $ onGroup W.focusDown')]

  -- Scratchpads
  -- Toggle show/hide these programs. They run on a hidden workspace.
  -- When you toggle them to show, it brings them to current workspace.
  -- Toggle them to hide and it sends them back to hidden workspace (NSP).
  ^++^ subKeys "Scratchpads"
  [ ("M-s t",       cmd "Toggle scratchpad terminal"              $ namedScratchpadAction myScratchPads "terminal")
  , ("M-s m",       cmd "Toggle scratchpad mocp"                  $ namedScratchpadAction myScratchPads "mocp")
  , ("M-s c",       cmd "Toggle scratchpad calculator"            $ namedScratchpadAction myScratchPads "calculator")]

  -- Controls for mocp music player (SUPER-u followed by a key)
  ^++^ subKeys "Mocp music player"
  [ ("M-u p",       cmd "mocp play"                               $ spawn "mocp --play")
  , ("M-u l",       cmd "mocp next"                               $ spawn "mocp --next")
  , ("M-u h",       cmd "mocp prev"                               $ spawn "mocp --previous")
  , ("M-u <Space>", cmd "mocp toggle pause"                       $ spawn "mocp --toggle-pause")]

  ^++^ subKeys "GridSelect"
  -- , ("C-g g", cmd "Select favorite apps"     $ runSelectedAction' defaultGSConfig gsCategories)
  [ ("M-M1-<Return>", cmd "Select favorite apps"                  $ spawnSelectedGrid gsCategories)
  , ("M-w",         cmd "Goto selected window"                    $ goToSelected  $ mygridConfig myGridColorizer)
  , ("M-S-w",       cmd "Bring selected window"                   $ bringSelected $ mygridConfig myGridColorizer)
  , ("M-M1-1",      cmd "1 Menu of games"                         $ spawnSelectedGrid gsGames)
  , ("M-M1-2",      cmd "2 Menu of education apps"                $ spawnSelectedGrid gsEducation)
  , ("M-M1-3",      cmd "3 Menu of Internet apps"                 $ spawnSelectedGrid gsInternet)
  , ("M-M1-4",      cmd "4 Menu of multimedia apps"               $ spawnSelectedGrid gsMultimedia)
  , ("M-M1-5",      cmd "5 Menu of office apps"                   $ spawnSelectedGrid gsOffice)
  , ("M-M1-6",      cmd "6 Menu of settings apps"                 $ spawnSelectedGrid gsSettings)
  , ("M-M1-7",      cmd "7 Menu of system apps"                   $ spawnSelectedGrid gsSystem)
  , ("M-M1-8",      cmd "8 Menu of utilities apps"                $ spawnSelectedGrid gsUtilities)]

  -- Emacs (SUPER-e followed by a key)
  ^++^ subKeys "Emacs"
  [ ("M-e e",       cmd "Emacsclient Everywhere"                  $ spawn (myEmacs ++ "--eval '(emacs-everywhere)'"))
  , ("M-e c",       cmd "Emacsclient"                             $ spawn myEmacs)
  , ("M-e a",       cmd "Emacsclient EMMS (music)"                $ spawn (myEmacs ++ "--eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/\")'"))
  , ("M-e b",       cmd "Emacsclient Ibuffer"                     $ spawn (myEmacs ++ "--eval '(ibuffer)'"))
  , ("M-e d",       cmd "Emacsclient Dired"                       $ spawn (myEmacs ++ "--eval '(dired nil)'"))
  , ("M-e s",       cmd "Emacsclient Eshell"                      $ spawn (myEmacs ++ "--eval '(eshell)'"))
  , ("M-e v",       cmd "Emacsclient Vterm"                       $ spawn (myEmacs ++ "--eval '(+vterm/here nil)'"))
  , ("M-e w",       cmd "Emacsclient EWW Browser"                 $ spawn (myEmacs ++ "--eval '(doom/window-maximize-buffer(eww \"distro.tube\"))'"))]
    where
      cmd = addName

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                                  $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'
