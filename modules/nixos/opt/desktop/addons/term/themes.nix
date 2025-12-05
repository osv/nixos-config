{ lib, ... }:

with lib;
{
  # Terminal color themes that can be shared across different terminal emulators
  themes = {
    dracula = rec {
      themeName = "Dracula";
      background = "#292d3e";
      foreground = "#bbc5ff";
      black = "#101010";
      red = "#f07178";
      green = "#c3e88d";
      yellow = "#ffcb6b";
      blue = "#82aaff";
      purple = "#c792ea";
      cyan = "#89ddff";
      white = "#d0d0d0";
      blackHi = "#737778";
      redHi = "#ff8b92";
      greenHi = "#ddffa7";
      yellowHi = "#ffe585";
      blueHi = "#e1acff";
      purpleHi = "#ef9ebe";
      cyanHi = "#a3f7ff";
      whiteHi = "#ffffff";
      mouseFg = blue;
      cursorFG = foreground;
      borderColor = background;
      selectionBg = blue;
      selectionFg = background;
    };
    light = rec {
      themeName = "Bluloco Light";
      # Base24 Bluloco Light from tinted-terminal
      # https://github.com/tinted-theming/tinted-terminal
      background = "#f7f7f7";  # light gray background
      foreground = "#38383a";  # dark gray foreground
      black = "#f7f7f7";       # ansi[0] - same as background
      red = "#c80d41";         # ansi[1]
      green = "#208839";       # ansi[2]
      yellow = "#1085d9";      # ansi[3] (actually cyan-ish)
      blue = "#1d44dd";        # ansi[4]
      purple = "#6d1bed";      # ansi[5]
      cyan = "#1e4d7a";        # ansi[6]
      white = "#000000";       # ansi[7] - black
      blackHi = "#bdbec8";     # brights[0] - light gray (patched)
      redHi = "#fb496d";       # brights[1]
      greenHi = "#34b253";     # brights[2]
      yellowHi = "#b79326";    # brights[3] - actual yellow
      blueHi = "#1085d9";      # brights[4]
      purpleHi = "#c00cb2";    # brights[5] - magenta
      cyanHi = "#5a7fac";      # brights[6]
      whiteHi = "#1c1d21";     # brights[7] - dark
      mouseFg = blue;
      cursorFG = "#38383a";    # cursor_border color
      borderColor = background;
      selectionBg = "#38383a"; # Official Bluloco Light selection background
      selectionFg = "#dddee8"; # Official Bluloco Light selection foreground
    };
    twentyTwo = rec {
      themeName = "Twenty-Two";
      background = "[80]rgb:00/00/00";
      foreground = "#bebebe";
      black = "#000000";
      red = "#d36265";
      green = "#aece91";
      yellow = "#e7e18c";
      blue = "#7a7ab0";
      purple = "#963c59";
      cyan = "#7f9f7f";
      white = "#bebebe";
      blackHi = "#666666";
      redHi = "#ef8171";
      greenHi = "#e5f779";
      yellowHi = "#f0dfaf";
      blueHi = "#8e9fbc";
      purpleHi = "#ef9ebe";
      cyanHi = "#71bebe";
      whiteHi = "#ffffff";
      mouseFg = blue;
      cursorFG = cyanHi;
      borderColor = background;
      selectionBg = blue;
      selectionFg = background;
    };
    gruvbox = rec {
      themeName = "Gruvbox Light";
      # Base24 Gruvbox Light from tinted-terminal
      # https://github.com/tinted-theming/tinted-terminal
      background = "#fbf1c7";  # warm light beige background
      foreground = "#3c3836";  # dark gray-brown foreground
      black = "#fbf1c7";       # ansi[0] - same as background
      red = "#cc241d";         # ansi[1]
      green = "#98971a";       # ansi[2]
      yellow = "#d79921";      # ansi[3]
      blue = "#458588";        # ansi[4]
      purple = "#b16286";      # ansi[5]
      cyan = "#689d6a";        # ansi[6]
      white = "#282828";       # ansi[7] - dark
      blackHi = "#d5c4a1";     # brights[0] - light brown
      redHi = "#9d0006";       # brights[1]
      greenHi = "#79740e";     # brights[2]
      yellowHi = "#b57614";    # brights[3]
      blueHi = "#076678";      # brights[4]
      purpleHi = "#8f3f71";    # brights[5]
      cyanHi = "#427b58";      # brights[6]
      whiteHi = "#1d2021";     # brights[7] - darkest
      mouseFg = blue;
      cursorFG = "#3c3836";    # cursor_border color
      borderColor = background;
      selectionBg = "#3c3836"; # Official selection background
      selectionFg = "#d5c4a1"; # Official selection foreground
    };
    dirty = rec {
      themeName = "Dirty";
      black = "#1D2021";
      red = "#cc251d";
      green = "#98971a";
      yellow = "#d79921";
      blue = "#458588";
      purple = "#b16286";
      cyan = "#689d6a";
      white = "#a89984";
      blackHi = "#928374";
      redHi = "#fb4934";
      greenHi = "#b8bb26";
      yellowHi = "#fabd2f";
      blueHi = "#83a598";
      purpleHi = "#d3869b";
      cyanHi = "#8ec07c";
      whiteHi = "#ebdbb2";
      background = black;
      foreground = white;
      mouseFg = blue;
      cursorFG = cyanHi;
      borderColor = background;
      selectionBg = blue;
      selectionFg = background;
    };
    rosePineDawn = rec {
      themeName = "Rose Pine Dawn";
      background = "#faf4ed";
      foreground = "#575279";
      black = "#f2e9e1";       # palette 0
      red = "#b4637a";         # palette 1
      green = "#286983";       # palette 2
      yellow = "#ea9d34";      # palette 3
      blue = "#56949f";        # palette 4
      purple = "#907aa9";      # palette 5
      cyan = "#d7827e";        # palette 6
      white = "#575279";       # palette 7
      blackHi = "#9893a5";     # palette 8
      redHi = "#b4637a";       # palette 9
      greenHi = "#286983";     # palette 10
      yellowHi = "#ea9d34";    # palette 11
      blueHi = "#56949f";      # palette 12
      purpleHi = "#907aa9";    # palette 13
      cyanHi = "#d7827e";      # palette 14
      whiteHi = "#575279";     # palette 15
      mouseFg = blue;
      cursorFG = "#575279";    # cursor-color
      borderColor = background;
      selectionBg = "#dfdad9"; # selection-background
      selectionFg = "#575279"; # selection-foreground
    };
  };
}
