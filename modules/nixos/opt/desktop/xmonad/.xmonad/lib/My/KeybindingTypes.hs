-- |
-- Module      : My.KeybindingTypes
-- Description : Data types and DSL for XMonad keybindings
--
-- This module provides structured data types for defining XMonad keybindings
-- that can be used both for XMonad configuration and HTML export.
--
-- == Single Source of Truth
--
-- Instead of parsing the string output of 'showKm', we define keybindings
-- as structured data that can generate both:
--
-- 1. XMonad format ('[((KeyMask, KeySym), NamedAction)]') for 'addDescrKeys''
-- 2. Export format ('KeybindingCategory') for HTML visualization
--
-- == Usage
--
-- Define keybindings using the DSL:
--
-- @
-- myKeybindingCategories :: [KeybindingCategoryDef]
-- myKeybindingCategories =
--   [ category "Xmonad Essentials" "xmonad-essentials"
--       [ bind "M-C-r" "Recompile XMonad" $ spawn "xmonad --recompile"
--       , bind "M-S-r" "Restart XMonad" $ spawn "xmonad --restart"
--       ]
--   ]
--   where
--     category = KeybindingCategoryDef
--     bind = KeybindingDef
-- @

module My.KeybindingTypes
  ( -- * Data types
    KeybindingCategory(..)
  , Keybinding(..)
  , KeyName(..)
  , KeybindingDef(..)
  , KeybindingCategoryDef(..)
    -- * DSL constructors
  , category
  , bind
    -- * Utilities
  , parseKeyNames
  , isChord
  , renderKeyName
  , findCategoryById
  ) where

import XMonad
import Data.List (groupBy)

-- | Export format for categories
data KeybindingCategory = KeybindingCategory
  { kcName     :: String  -- ^ Display name
  , kcId       :: String  -- ^ URL-safe ID
  , kcBindings :: [Keybinding]
  } deriving (Show, Eq)

-- | Export format for individual binding
data Keybinding = Keybinding
  { kbCombo   :: String        -- ^ Original combo (e.g., "M-C-r")
  , kbKeys    :: [KeyName]     -- ^ Parsed keys
  , kbAction  :: String        -- ^ Description
  , kbIsChord :: Bool          -- ^ Chord binding
  , kbCatId   :: Maybe String  -- ^ Category ID (filled during export)
  } deriving (Show, Eq)

-- | Parsed key representation
data KeyName
  = Super | Ctrl | Shift | Alt
  | KeyChar Char
  | SpecialKey String
  deriving (Show, Eq)

-- | DSL for defining individual bindings
data KeybindingDef = KeybindingDef
  { kdCombo  :: String   -- ^ Key combination string (e.g., "M-C-r")
  , kdDesc   :: String   -- ^ Description for help
  , kdAction :: X ()     -- ^ Action to execute
  }

-- | DSL for defining categories of bindings
data KeybindingCategoryDef = KeybindingCategoryDef
  { kcdName     :: String              -- ^ Display name
  , kcdId       :: String              -- ^ URL-safe ID
  , kcdBindings :: [KeybindingDef]    -- ^ List of bindings
  }

-- | Smart constructor for KeybindingCategoryDef
category :: String -> String -> [KeybindingDef] -> KeybindingCategoryDef
category = KeybindingCategoryDef

-- | Smart constructor for KeybindingDef
bind :: String -> String -> X () -> KeybindingDef
bind = KeybindingDef

-- | Parse key combination string into KeyName list
--
-- Examples:
-- * "M-C-r" → [Super, Ctrl, KeyChar 'r']
-- * "M-S-<Left>" → [Super, Shift, SpecialKey "Left"]
-- * "M-s t" → [Super, KeyChar 's', KeyChar 't'] (chord binding)
-- * "M--" → [Super, KeyChar '-']  (double hyphen means literal hyphen)
parseKeyNames :: String -> [KeyName]
parseKeyNames combo =
  case words combo of
    [modPart, finalKey] ->
      -- Check if modPart is a chord trigger (e.g., "M-s", "M-e", "M-u")
      case parseModPart modPart of
        Just mods -> mods ++ [parseSingleKey finalKey]
        Nothing -> parseCombo combo
    _ ->
      -- Regular binding
      parseCombo combo
  where
    -- Parse combo, treating "--" as a single hyphen key
    parseCombo str = map parseKeyPart (splitDash str)

    -- Split by dash, where "--" is treated as a single token
    -- Single dashes are separators (discarded)
    -- "M-C-r" → ["M", "C", "r"]
    -- "M--" → ["M", "--"]
    splitDash str = filter (not . isSeparator) (processGroups (groupBy (\a b -> a == '-' && b == '-') str))
      where
        processGroups groups = map extractToken groups
        extractToken g
          | length g >= 2 && head g == '-' = "--"  -- Double hyphen = literal minus key
          | length g == 1 && head g == '-' = "-"    -- Single dash = separator (will be filtered)
          | otherwise = [head g]                     -- Other characters
        isSeparator s = s == "-"

    -- Parse modifier part like "M-s" into [Super] + chord key
    parseModPart str =
      case splitOn "-" str of
        ["M", k] | length k == 1 -> Just [Super, KeyChar (head k)]
        ["M4", k] | length k == 1 -> Just [Super, KeyChar (head k)]
        _ -> Nothing

    parseSingleKey "<Return>" = SpecialKey "Enter"
    parseSingleKey "<Space>" = SpecialKey "Space"
    parseSingleKey ('<':xs) | not (null xs) = SpecialKey (init xs)
    parseSingleKey [c] = KeyChar c
    parseSingleKey other = SpecialKey other

    parseKeyPart "M" = Super
    parseKeyPart "M4" = Super
    parseKeyPart "C" = Ctrl
    parseKeyPart "S" = Shift
    parseKeyPart "M1" = Alt
    parseKeyPart "--" = KeyChar '-'  -- Double hyphen means literal hyphen key
    parseKeyPart ('<':xs) | not (null xs) = SpecialKey (init xs)  -- Handle <Left>, <Return>, <F1>, etc.
    parseKeyPart [c] = KeyChar c
    parseKeyPart other = SpecialKey other

-- | Check if combo is a chord binding (e.g., "M-s t")
--
-- Chord bindings have 2 parts when split by words: mod+key and final key
isChord :: String -> Bool
isChord combo = case words combo of
  [modPart, _] ->
    -- Check if modPart is like "M-s" or "M4-s" (mod + single char)
    case splitOn "-" modPart of
      ["M", k] -> length k == 1
      ["M4", k] -> length k == 1
      _ -> False
  _ -> False

-- | Render KeyName to string for display
renderKeyName :: KeyName -> String
renderKeyName Super = "Super"
renderKeyName Ctrl = "Ctrl"
renderKeyName Shift = "Shift"
renderKeyName Alt = "Alt"
renderKeyName (KeyChar c) = [c]
renderKeyName (SpecialKey k) = k

-- | Split string by delimiter
-- Handles repeated delimiters correctly
splitOn :: String -> String -> [String]
splitOn _ "" = []
splitOn delim str =
  case breakOn delim str of
    (before, rest) ->
      if null rest
        then [before]
        else before : splitOn delim (drop (length delim) rest)

-- | Break string at first occurrence of delimiter
-- Returns (before, including_delimiter_and_after)
breakOn :: String -> String -> (String, String)
breakOn delim str
  | delim `isPrefixOf` str = ([], str)
  | otherwise = case str of
      [] -> ([], [])
      (c:cs) ->
        let (before, after) = breakOn delim cs
        in (c:before, after)

-- | Find index of first element satisfying predicate
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex p (x:xs)
  | p x = Just 0
  | otherwise = (1 +) <$> findIndex p xs

-- | Check if first list is prefix of second
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | All tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

-- | Find category by ID (specialized for KeybindingCategory)
findCategoryById :: String -> [KeybindingCategory] -> Maybe Int
findCategoryById targetId cats = findIndex (\c -> kcId c == targetId) cats
