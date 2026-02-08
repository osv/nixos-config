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
  , KeybindingDef(..)
  , KeybindingCategoryDef(..)
    -- * DSL constructors
  , category
  , bind
  ) where

import XMonad

-- | Export format for categories
data KeybindingCategory = KeybindingCategory
  { kcName     :: String  -- ^ Display name
  , kcId       :: String  -- ^ URL-safe ID
  , kcBindings :: [Keybinding]
  } deriving (Show, Eq)

-- | Export format for individual binding
data Keybinding = Keybinding
  { kbCombo   :: String        -- ^ Original combo (e.g., "M-C-r")
  , kbAction  :: String        -- ^ Description
  } deriving (Show, Eq)

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

