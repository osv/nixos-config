module My.Xprop (xprop, xpropOrDefault) where

import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Char (isSpace, toUpper)
import Data.List ( find, elemIndex, dropWhileEnd )
import Data.Maybe ( fromMaybe, mapMaybe, catMaybes )
import System.IO.Unsafe (unsafeDupablePerformIO)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)

-- >>> xProperty "xmonad.terminal"
-- "urxvt"
xProperty :: String -- ^
  -> IO String
xProperty key = fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""

-- >>> findValue "*.foo" "*.foo:   abc"
-- Just "abc"
-- >>> findValue "*.bar" "*.foo:   abc"
-- Nothing
findValue :: String -> String -> Maybe String
findValue xresKey xres = snd <$> find ((== xresKey) . fst) (mapMaybe splitAtColon (lines xres))

-- >>> splitAtColon "foo:bar"
-- Just ("foo","bar")
splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming str idx = bimap trim' (trim' . tail) $ splitAt idx str

trim', xprop :: ShowS
trim' = dropWhileEnd isSpace . dropWhile isSpace
xprop = unsafeDupablePerformIO . xProperty

-- >>> xpropOrDefault "SomeDefault" "xmonad.terminal"
-- "urxvt"
-- >>> xpropOrDefault "SomeDefault" "xmonad.foo"
-- "SomeDefault"
xpropOrDefault :: String -> String -> String
xpropOrDefault defaultValue key =
  let propResult = xprop key
  in bool propResult defaultValue (null propResult)
