module XMonad.Hooks.PrettyStatusBar where
import XMonad
import Data.List (elemIndex, find, sortOn)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Actions.WorkspaceNames ( getWorkspaceName )

-- | Datatype for expanded 'Icon' configurations
data PSBConfig = PSBConfig
    { psbOutput :: String -> IO ()
      -- ^ Output workspace, layout and title.
    , psbIcons :: M.Map String String
    , fg :: String -> String -> String
    , bg :: String -> String -> String
    }


instance Default PSBConfig where
    def = PSBConfig
        { psbOutput = putStrLn
        , psbIcons = M.fromList []
        , fg = \_ -> id
        , bg = \_ -> id
        }

polybarPSB :: PSBConfig
polybarPSB = def { fg = polybarFG
                 , bg = polybarBG
                 }

prettyStatusBarConfigdynamicIconsPP :: PSBConfig -> X ()
prettyStatusBarConfigdynamicIconsPP psbConfig = prettyStatusBarString psbConfig >>= io . psbOutput psbConfig

prettyStatusBarString :: PSBConfig -> X String
prettyStatusBarString psbConfig = joinWithSpaces 1 <$> prettyWorkspaceList psbConfig

joinWithSpaces :: Int -> [String] -> String
joinWithSpaces spaces = joinStrings $ replicate spaces ' '

prettyWorkspaceList :: PSBConfig -> X [String]
prettyWorkspaceList cfg = do
  curWorkspaces <- sortOn W.tag . filter (\wspace -> W.tag wspace /= "NSP") . W.workspaces . windowset <$> get
  workspaceIcons <- filter (\(_, icons) -> not $ null icons) . zip (map W.tag curWorkspaces) <$> mapM (prettyWindowIconList cfg) curWorkspaces
  focusedWspace <- W.tag . W.workspace . W.current . windowset <$> get
  let colour tag = (if tag == focusedWspace then highlight cfg else normal cfg)
  return $ map (\(tag, winIcons) -> colour tag  $ joinWithSpaces 1 ( tag : winIcons)) workspaceIcons

wsName :: WorkspaceId -> X String
wsName t = do
  n <- getWorkspaceName t
  return $ fromMaybe "" n

polybarClickable :: WorkspaceId -> String -> X String
polybarClickable tag iconsStr = do
  n <- wsName tag
  return $ "%{A:xdotool key Super+" ++ n ++ ":}" ++ iconsStr ++ "%{A}"

highlight :: PSBConfig -> String -> String
highlight cfg = fg cfg "#FFFFFF"

normal :: PSBConfig -> String -> String
normal cfg = fg cfg "#777777"

polybarFG :: String -> String -> String
polybarFG colour str = "%{F" <> colour <> "}" <> str <> "%{F-}"

polybarBG :: String -> String -> String
polybarBG colour str = "%{B" <> colour <> "}" <> str <> "%{B-}"

underline :: String -> String -> String
underline colour str = "%{u" <> colour <> "}" <> str <> "%{u-}"

joinStrings :: String -> [String] -> String
joinStrings joinWith = foldr (\a b -> a <> joinWith <> b) ""

prettyWindowIconList :: PSBConfig -> W.Workspace WorkspaceId l Window -> X [String]
prettyWindowIconList cfg workspace = case W.stack workspace of
  Nothing -> return []
  Just curStack -> do
    let curWindows = W.integrate curStack
    winIcons <- windowIcons cfg curWindows
    let focusedIndex = fromMaybe (-1) $ elemIndex (W.focus curStack) curWindows
    isWorkspaceFocused <- (==) (W.tag workspace) . W.tag . W.workspace . W.current . windowset <$> get
    return $ zipWith (\icon i -> if i == focusedIndex && isWorkspaceFocused then highlight cfg icon else normal cfg icon) winIcons [0 ..]

windowIcons :: PSBConfig -> [Window] -> X [String]
windowIcons cfg winIds = do
  dis <- display <$> ask
  windowClasses <- io $ mapM (fmap resClass . getClassHint dis) winIds
  return $ map (\win -> fromMaybe "\xf2d0" (M.lookup win (psbIcons cfg))) windowClasses
