{ lib, ... }:
with lib;
rec {
  urxvtHelper = rec {
    esc = cmd: value: "\\033]${toString cmd};${value}\\007";
    setColor = colorIndex: color: esc 4 "${toString colorIndex};${color}";
    setBlack = color: setColor 0 color;
    setRed = color: setColor 1 color;
    setGreen = color: setColor 2 color;
    setYellow = color: setColor 3 color;
    setBlue = color: setColor 4 color;
    setPurple = color: setColor 5 color;
    setCyan = color: setColor 6 color;
    setWhite = color: setColor 7 color;
    setBlackHi = color: setColor 8 color;
    setRedHi = color: setColor 9 color;
    setGreenHi = color: setColor 10 color;
    setYellowHi = color: setColor 11 color;
    setBlueHi = color: setColor 12 color;
    setPurpleHi = color: setColor 13 color;
    setCyanHi = color: setColor 14 color;
    setWhiteHi = color: setColor 15 color;

    setFg = color: esc 10 color;
    setBg = color: esc 11 color;
    setCursorFg = color: esc 12 color;
    setMouseFg = color: esc 13 color;
    setBorderColor = color: esc 708 color;

    setItalicColor = color: esc 704 color;
    setBoldColor = color: esc 706 color;
    setUnderlineColor = color: esc 707 color;

    setFont = fn: esc 710 fn;
    setBoldFont = fn: esc 711 fn;
    setItalicFont = fn: esc 712 fn;
    setBoldItalicFont = fn: esc 713 fn;

    command = commands: "command:${strings.concatStringsSep "\\\n" commands}";

    baseTheme = rec {
      background = "#000000";
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
      mouseFG = blue;
      cursorFG = foreground;
      borderColor = background;
    };

    # Generates an Xresources keybinding command to switch URxvt theme dynamically.
    xresourcesChangeTheme = keybinding: theme:
      "urxvt*keysym.${keybinding}: " + (command [
        (setFg theme.foreground)
        (setBg theme.background)
        (setBorderColor theme.borderColor)
        (setBlack theme.black)
        (setRed theme.red)
        (setGreen theme.green)
        (setYellow theme.yellow)
        (setBlue theme.blue)
        (setPurple theme.purple)
        (setCyan theme.cyan)
        (setWhite theme.white)
        (setBlackHi theme.blackHi)
        (setRedHi theme.redHi)
        (setGreenHi theme.greenHi)
        (setYellowHi theme.yellowHi)
        (setBlueHi theme.blueHi)
        (setPurpleHi theme.purpleHi)
        (setCyanHi theme.cyanHi)
        (setWhiteHi theme.whiteHi)
        (setMouseFg theme.mouseFG)
        (setCursorFg theme.cursorFG)
      ])
      + "\n";

    # Generates Xresources theme declarations with a given prefix (e.g., "urxvt.mytheme.")
    xresourcesTheme = name: theme:
      ''
${name}color0:       ${theme.black}
${name}color1:       ${theme.red}
${name}color2:       ${theme.green}
${name}color3:       ${theme.yellow}
${name}color4:       ${theme.blue}
${name}color5:       ${theme.purple}
${name}color6:       ${theme.cyan}
${name}color7:       ${theme.white}

${name}color8:       ${theme.blackHi}
${name}color9:       ${theme.redHi}
${name}color10:      ${theme.greenHi}
${name}color11:      ${theme.yellowHi}
${name}color12:      ${theme.blueHi}
${name}color13:      ${theme.purpleHi}
${name}color14:      ${theme.cyanHi}
${name}color15:      ${theme.whiteHi}

${name}background:   ${theme.background}
${name}foreground:   ${theme.foreground}
${name}cursorColor:  ${theme.cursorFG}
${name}borderColor:  ${theme.borderColor}
      '';

    # Builds keybinding commands for switching to each theme defined in `themes`.
    xresourcesChangeThemesCommands = themes:
      strings.concatLines (lib.mapAttrsToList
        (key: theme: (xresourcesChangeTheme key (baseTheme // theme)) + "\n")
        themes);

    # Generates Xresources definitions for all URxvt themes with namespaced theme settings.
    xresourcesUrxvtThemes = themes:
      strings.concatLines (lib.mapAttrsToList
        (key: theme: (xresourcesTheme "${theme.themeName}." (baseTheme // theme)) + "\n")
        themes);

    # Defines the default URxvt theme using wildcard `urxvt*` in Xresources.
    defaultTheme = theme:
      (xresourcesTheme "URxvt*" (baseTheme // theme)) + "\n";
  };
}
