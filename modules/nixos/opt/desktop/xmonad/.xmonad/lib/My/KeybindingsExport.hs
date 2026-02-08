{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : My.KeybindingsExport
-- Description : Export XMonad keybindings to HTML visualization
--
-- This module exports XMonad keybindings to a self-contained HTML file
-- that can be opened in a web browser.
--
-- == How it works
--
-- 1. On XMonad startup, 'exportKeybindings' is called from the startup hook
-- 2. It reads the HTML template from @~/.xmonad/data/keybinding-template.html@
-- 3. It extracts categories and keybindings from 'allKeysStructured'
-- 4. Inline JavaScript is generated with appConfig, categories, and keybindings
-- 5. The HTML is written to @~/.cache/nixos-config/keybinding/xmonad/index.html@
-- 6. The main index page is regenerated via 'my-generate-keybindings-index'
--
-- == Zero Parsing Approach
--
-- Unlike the old version that parsed 'showKm' string output, this version
-- uses structured data directly from 'allKeysStructured'. No parsing needed!
--
-- == Template Integration
--
-- The shared template (@lib/keybinding-template.html@) handles color assignment,
-- chord detection, and key parsing. This module only needs to emit simple data.
--
-- == Usage
--
-- Add 'exportKeybindings' to the startup hook in @My.Main@:
--
-- @
-- import My.KeybindingsExport
--
-- myStartupHook :: X ()
-- myStartupHook = do
--   exportKeybindings
--   -- ... other startup actions
-- @

module My.KeybindingsExport (exportKeybindings) where

import XMonad
import My.Keybindings (allKeysStructured)
import My.KeybindingTypes (KeybindingCategory(..), Keybinding(..))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)
import Data.List (intercalate)

-- | Export keybindings to HTML file
--
-- This is the main entry point. Call this from your startup hook.
exportKeybindings :: X ()
exportKeybindings = do
  -- Ensure output directories exist
  io $ ensureOutputDirs

  -- Read HTML template
  template <- io $ readTemplate

  -- Use structured data directly - NO PARSING!
  let categories = allKeysStructured
  let script = generateInlineScript categories

  -- Insert script by replacing placeholder
  let finalHTML = insertScript template script

  -- Write output file
  io $ writeHTMLFile finalHTML

  -- Regenerate main index page
  io $ regenerateMainIndex

-- | Ensure output directories exist
ensureOutputDirs :: IO ()
ensureOutputDirs = do
  home <- getHomeDirectory
  let hotkeyDir = home </> ".cache" </> "nixos-config" </> "keybinding" </> "xmonad"
  createDirectoryIfMissing True hotkeyDir

-- | Read the HTML template
readTemplate :: IO String
readTemplate = do
  home <- getHomeDirectory
  let templatePath = home </> ".xmonad" </> "data" </> "keybinding-template.html"
  readFile templatePath

-- | Write the final HTML file
writeHTMLFile :: String -> IO ()
writeHTMLFile content = do
  home <- getHomeDirectory
  let outputPath = home </> ".cache" </> "nixos-config" </> "keybinding" </> "xmonad" </> "index.html"
  writeFile outputPath content

-- | Regenerate the main keybindings index page
regenerateMainIndex :: IO ()
regenerateMainIndex = callCommand "my-generate-keybindings-index"

-- | Generate inline JavaScript with appConfig, categories, and keybindings data
--
-- Emits the data format expected by the shared keybinding-template.html:
--   const appConfig = { title, modifiers };
--   const categories = { 'id': 'Display Name', ... };
--   const keybindings = [ ['combo', 'category-id', 'action'], ... ];
generateInlineScript :: [KeybindingCategory] -> String
generateInlineScript cats =
  "const appConfig = {\n" ++
  "    title: 'XMonad',\n" ++
  "    modifiers: { 'M': 'Super', 'C': 'Ctrl', 'S': 'Shift', 'M1': 'Alt', 'M4': 'Super' }\n" ++
  "};\n" ++
  "const categories = {\n" ++
  intercalate ",\n" (map categoryEntry cats) ++
  "\n};\n" ++
  "const keybindings = [\n" ++
  intercalate ",\n" (concatMap categoryBindings cats) ++
  "\n];\n"
  where
    categoryEntry cat =
      "    '" ++ escapeJS (kcId cat) ++ "': '" ++ escapeJS (kcName cat) ++ "'"

    categoryBindings cat =
      map (bindingEntry (kcId cat)) (kcBindings cat)

    bindingEntry catId b =
      "    ['" ++ escapeJS (kbCombo b) ++ "', '" ++ escapeJS catId ++ "', '" ++ escapeJS (kbAction b) ++ "']"

    -- Escape JavaScript strings (basic)
    -- Note: HTML escaping is done in the template via escapeHtml() function
    escapeJS = concatMap $ \case
      '\\' -> "\\\\"
      '"' -> "\\\""
      '\'' -> "\\'"
      '\n' -> "\\n"
      c -> [c]

-- | Insert script content by replacing the template placeholder
insertScript :: String -> String -> String
insertScript template script =
  let placeholder = "// app_keybinding_data"
  in case breakOn placeholder template of
       (before, after) ->
         if null after
         then template ++ script  -- Fallback if placeholder not found
         else before ++ script ++ drop (length placeholder) after

-- | Break string at first occurrence of delimiter
breakOn :: String -> String -> (String, String)
breakOn delim str =
  case findIndexHelper (isPrefixOfHelper delim) str of
    Nothing -> (str, "")
    Just idx -> splitAtHelper idx str

-- | Find index in string
findIndexHelper :: (String -> Bool) -> String -> Maybe Int
findIndexHelper _ [] = Nothing
findIndexHelper p str
  | p str = Just 0
  | otherwise = (1 +) <$> findIndexHelper p (tail str)

-- | Check if first string is prefix of second
isPrefixOfHelper :: String -> String -> Bool
isPrefixOfHelper [] _ = True
isPrefixOfHelper _ [] = False
isPrefixOfHelper (x:xs) (y:ys) = x == y && isPrefixOfHelper xs ys

-- | Split string at index
splitAtHelper :: Int -> String -> (String, String)
splitAtHelper n str = (take n str, drop n str)
