{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.xmonad;
in
{
  options.nerv.opt.desktop.xmonad = with types; {
    enable = mkBoolOpt false "Whether or not to enable Xmonad.";
  };

  config = mkIf cfg.enable {
    # https://gvolpe.com/blog/xmonad-polybar-nixos/

    nerv = {
      opt.desktop = {
        xmonad.polybar = on;
        addons = {
          picom = on;
          notify = on;
          rofi = on;
          xresources = on;

          wallpapers = on;
          wallpaper = on;
          keyring = on;
          electron-support = on;

          boomer = on;
        };
      };

      opt.persist.derivative.homeDirectories = [ ".xmonad" ];

      home = {
        file = {
          ".xmonad/bin" = { source = ./.xmonad/bin; recursive = true; };
          ".xmonad/lib" = { source = ./.xmonad/lib; recursive = true; };
          ".xmonad/xmonad.hs" = {
            source = ./.xmonad/xmonad.hs;
          };
          ".xmonad/hotkey-template.html" = {
            source = ./.xmonad/hotkey-template.html;
          };
        };
      };
    };

    programs.dconf.enable = true;

    services = {
      dbus = {
        enable = true;
        packages = [ pkgs.dconf ];
      };

      xserver = {
        windowManager.xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = hp: [ hp.dbus hp.monad-logger ];
        };
      };
    };

    environment.systemPackages = with pkgs; [
      killall
      polybarFull
      zenity # M-F1 Help Dialog
      setroot
      arandr
      xzoom
      dzen2 # for showing help by M-F1
      xorg.xwininfo # emacs everywhere
      xclip
      xdotool
      scrot
      ksnip # screenshotter, when I need to draw arrow
      flameshot # another screenshotter
      pkgs.nerv.my-generate-keybindings-index # keybindings documentation generator

      ## Test config script
      (mkTestConfigScript pkgs {
        name = "my-test-xmonad-config";
        appName = "XMonad";
        sourcePath = "modules/nixos/opt/desktop/xmonad/.xmonad";
        targetPath = ".xmonad";
        files = [ "xmonad.hs" "bin" "lib" "hotkey-template.html"];
        reloadCmd = "xmonad --recompile && Mod+q";
      })
    ];
  };
}
