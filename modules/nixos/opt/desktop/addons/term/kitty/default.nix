{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.term.kitty;
  
  # Import shared themes
  themeDefinitions = import ../themes.nix { inherit lib; };
  kittyTheme = themeDefinitions.themes.dracula;
  
  # Kitty configuration with Dracula theme
  kittyConfig = ''
    # Font configuration
    font_family      JetBrains Mono
    bold_font        auto
    italic_font      auto
    bold_italic_font auto
    font_size        11.0

    # Cursor configuration
    cursor_shape     underline
    cursor_blink_interval 0.5

    # Window configuration
    window_padding_width 10

    # Tab bar configuration
    tab_bar_edge bottom
    tab_bar_style powerline

    # Dracula color theme
    foreground ${kittyTheme.foreground}
    background ${kittyTheme.background}
    cursor     ${kittyTheme.cursorFG}

    # Black
    color0  ${kittyTheme.black}
    color8  ${kittyTheme.blackHi}

    # Red
    color1  ${kittyTheme.red}
    color9  ${kittyTheme.redHi}

    # Green
    color2  ${kittyTheme.green}
    color10 ${kittyTheme.greenHi}

    # Yellow
    color3  ${kittyTheme.yellow}
    color11 ${kittyTheme.yellowHi}

    # Blue
    color4  ${kittyTheme.blue}
    color12 ${kittyTheme.blueHi}

    # Magenta
    color5  ${kittyTheme.purple}
    color13 ${kittyTheme.purpleHi}

    # Cyan
    color6  ${kittyTheme.cyan}
    color14 ${kittyTheme.cyanHi}

    # White
    color7  ${kittyTheme.white}
    color15 ${kittyTheme.whiteHi}

    # Key mappings
    map ctrl+shift+f search_forward
    map ctrl+0       change_font_size all 0
    map ctrl+plus    change_font_size all +2.0
    map ctrl+minus   change_font_size all -2.0

    # Performance tuning
    repaint_delay   10
    input_delay     3
    sync_to_monitor yes

    # Terminal bell
    enable_audio_bell no
    visual_bell_duration 0.0

    # Advanced
    allow_remote_control no
    shell_integration enabled
  '';
in {
  options.nerv.opt.desktop.addons.term.kitty = with types; {
    enable = mkBoolOpt false "Enable Kitty terminal emulator";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.kitty ];
    
    # Write Kitty configuration
    nerv.home.configFile."kitty/kitty.conf".text = kittyConfig;
    
    # Persistence configuration for Kitty
    nerv.opt.persist = {
      state.homeDirectories = [ ".config/kitty" ];
    };
  };
}
