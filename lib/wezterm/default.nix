{ lib, ... }:
with lib; rec {
  weztermHelper = rec {
    # Convert X11/urxvt color format to WezTerm-compatible format
    # Handles special formats like "[80]rgb:00/00/00" with transparency
    normalizeColor = color:
      if strings.hasPrefix "[" color then
      # Parse [percent]rgb:RR/GG/BB format
      # Example: [80]rgb:00/00/00 -> rgba(0, 0, 0, 0.8)
        let
          # Extract opacity percentage more efficiently
          bracketPart = builtins.elemAt (strings.splitString "]" color) 0;
          opacityStr =
            strings.substring 1 (strings.stringLength bracketPart - 1)
            bracketPart;
          opacity = strings.toInt opacityStr;
          alpha = toString (opacity * 1.0e-2);

          # Extract RGB components
          rgbPart = builtins.elemAt (strings.splitString "rgb:" color) 1;
          components = strings.splitString "/" rgbPart;

          # Convert hex to decimal using character arithmetic
          hexToDec = hex:
            let
              chars = strings.stringToCharacters (strings.toLower hex);
              # Convert single hex character to integer using ASCII arithmetic
              charToInt = c:
                let
                  code = strings.charToInt c;
                  # 'a'-'f' (97-102) -> 10-15
                  # '0'-'9' (48-57) -> 0-9
                in if code >= 97 then code - 87 else code - 48;
              # Accumulate value for any length hex string
              accumulate = acc: char: acc * 16 + charToInt char;
            in toString (foldl' accumulate 0 chars);

          r = hexToDec (builtins.elemAt components 0);
          g = hexToDec (builtins.elemAt components 1);
          b = hexToDec (builtins.elemAt components 2);
        in "rgba(${r}, ${g}, ${b}, ${alpha})"
      else
        color;

    # Extract opacity from color format like [80]rgb:00/00/00
    extractOpacity = color:
      if strings.hasPrefix "[" color then
        let
          bracketPart = builtins.elemAt (strings.splitString "]" color) 0;
          opacityStr =
            strings.substring 1 (strings.stringLength bracketPart - 1)
            bracketPart;
        in strings.toInt opacityStr * 1.0e-2
      else
        1.0;

    # Convert a theme from our shared format to WezTerm's Lua color scheme format
    themeToLua = theme:
      let
        # Get opacity from background color, default to 1.0
        opacity = extractOpacity theme.background;
        # Use explicit selection colors if provided, otherwise fall back to blue/background
        selBg = theme.selectionBg or theme.blue;
        selFg = theme.selectionFg or theme.background;
      in ''
        return {
          opacity = ${toString opacity},  -- Window transparency level
          foreground = '${normalizeColor theme.foreground}',
          background = '${normalizeColor theme.background}',
          cursor_bg = '${normalizeColor theme.cursorFG}',
          cursor_border = '${normalizeColor theme.cursorFG}',
          cursor_fg = '${normalizeColor theme.background}',
          selection_bg = '${normalizeColor selBg}',
          selection_fg = '${normalizeColor selFg}',

          ansi = {
            '${normalizeColor theme.black}',   -- black
            '${normalizeColor theme.red}',     -- red
            '${normalizeColor theme.green}',   -- green
            '${normalizeColor theme.yellow}',  -- yellow
            '${normalizeColor theme.blue}',    -- blue
            '${normalizeColor theme.purple}',  -- purple/magenta
            '${normalizeColor theme.cyan}',    -- cyan
            '${normalizeColor theme.white}',   -- white
          },

          brights = {
            '${normalizeColor theme.blackHi}',  -- black
            '${normalizeColor theme.redHi}',    -- red
            '${normalizeColor theme.greenHi}',  -- green
            '${normalizeColor theme.yellowHi}', -- yellow
            '${normalizeColor theme.blueHi}',   -- blue
            '${normalizeColor theme.purpleHi}', -- purple/magenta
            '${normalizeColor theme.cyanHi}',   -- cyan
            '${normalizeColor theme.whiteHi}',  -- white
          },
        }
      '';

    # Generate a Lua file for a single theme
    generateThemeFile = name: theme: {
      name = "wezterm/themes/${name}.lua";
      value.text = themeToLua theme;
    };

    # Generate all theme files from a set of themes
    generateThemeFiles = themes:
      lib.listToAttrs (lib.mapAttrsToList generateThemeFile themes);
  };
}
