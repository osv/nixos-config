module My.ServerEventHook (myServerModeEventHook) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ServerMode
import My.Theme

-----------------------------------------------------------------------
-- Custom server hook for xmonadctl with workspace commands (focus-workspace-www) and more
myServerModeEventHook = serverModeEventHookCmd' $ return myCommandsWithWsc'

myCommandsWithWsc' :: [(String, X ())]
myCommandsWithWsc' = ("help", listMyServerCmds) : myCommands ++ wscs -- ++ sccs -- ++ spcs
  where
    wscs = [( m ++ s, windows $ f s) | s <- myWorkspaces
            , (f, m) <- [(W.view, "focus-workspace-"), (W.shift, "send-to-workspace-")] ]
    -- TODO Add screens support?
    -- sccs = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
    --        | sc <- [0..myMaxScreenCount], (f, m) <- [(W.view, "focus-screen-"), (W.shift, "send-to-screen-")]]
    --        spcs = [("toggle-" ++ sp, namedScratchpadAction myScratchpads sp)
    --               | sp <- (flip map) (myScratchpads) (\(NS x _ _ _) -> x) ]

------------------------------------------------------------------------
-- External commands  for xmonadctl
myCommands :: [(String, X ())]
myCommands =
  [ ("next-layout"               , sendMessage NextLayout                           )
  , ("default-layout"            , asks (layoutHook . config) >>= setLayout         )
  ]

------------------------------------------------------------------------
-- Help command
listMyServerCmds :: X ()
listMyServerCmds = spawn ("echo '" ++ asmc ++ "' | " ++ myShowPipedApp)
    where asmc = concat $ "Available commands:\n" : map (\(x, _)-> "    " ++ x ++ "\n") myCommandsWithWsc'
