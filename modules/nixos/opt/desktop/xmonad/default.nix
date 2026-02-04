{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.xmonad;
  userName = config.nerv.opt.user.name;
  homeDir = "/home/${userName}";
  xmonadDir = "${homeDir}/.xmonad";

  # Source paths in nix store
  srcBin = ./.xmonad/bin;
  srcLib = ./.xmonad/lib;
  srcXmonadHs = ./.xmonad/xmonad.hs;
  srcTemplate = ./.xmonad/hotkey-template.html;
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

      # Persist whole .xmonad directory (binary + build artifacts)
      # Config files are copied via activation script with rsync --checksum
      # so mtime only updates when content changes → no unnecessary recompilation
      opt.persist.derivative.homeDirectories = [ ".xmonad" ];
    };

    # Copy xmonad config files using rsync --checksum
    # Only updates files if content changed, preserving mtime otherwise
    # This allows XMonad to skip recompilation when config hasn't changed
    # Uses systemd service to run AFTER impermanence mounts .xmonad directory
    systemd.services.xmonad-config-sync = {
      description = "Sync XMonad config files";
      wantedBy = [ "multi-user.target" ];
      # Run after the .xmonad bind mount from impermanence
      after = [ "home-${userName}-.xmonad.mount" ];
      requires = [ "home-${userName}-.xmonad.mount" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "xmonad-config-sync" ''
          echo "[xmonad] Syncing XMonad config..."
          mkdir -p ${xmonadDir}/bin ${xmonadDir}/lib
          ${pkgs.rsync}/bin/rsync -r --checksum --delete ${srcBin}/ ${xmonadDir}/bin/
          ${pkgs.rsync}/bin/rsync -r --checksum --delete ${srcLib}/ ${xmonadDir}/lib/
          ${pkgs.rsync}/bin/rsync --checksum ${srcXmonadHs} ${xmonadDir}/xmonad.hs
          ${pkgs.rsync}/bin/rsync --checksum ${srcTemplate} ${xmonadDir}/hotkey-template.html
          chown -R ${userName}:users ${xmonadDir}
        '';
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
