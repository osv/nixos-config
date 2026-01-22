{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.screenlock;

  # Lock script with all options
  # Used by xss-lock, xidlehook, and can be called from XMonad (M-l keybinding)
  # Options after -- are passed to i3lock-color
  lockScript = pkgs.writeShellScriptBin "my-screenlock" ''
    # Switch to English layout before locking (for password entry)
    ${pkgs.xkb-switch}/bin/xkb-switch -s us
    exec ${pkgs.betterlockscreen}/bin/betterlockscreen \
      -l ${cfg.effect} \
      -- --indicator \
      ${optionalString cfg.showLayout "--keylayout 1"}
  '';
in {
  options.nerv.opt.desktop.addons.screenlock = with types; {
    enable = mkBoolOpt false "Whether to enable screen lock (betterlockscreen + xidlehook).";
    inactiveInterval = mkOpt int 20 "Minutes of inactivity before lock.";
    notifyInterval = mkOpt int 2 "Minutes before lock/suspend to show notification.";
    suspendInterval = mkOpt int 30 "Minutes after lock before suspend (0 to disable).";
    effect = mkOpt str "dim" "Lock screen effect (dim, blur, dimblur, pixel, color).";
    showLayout = mkBoolOpt true "Show current keyboard layout on lock screen (Caps Lock shown by default).";
    wallpaper = mkOpt (nullOr (either str path)) null "Wallpaper image path for lock screen (string or path). If null, must run 'betterlockscreen -u <path>' manually.";
  };

  config = mkIf cfg.enable {
    # PAM for i3lock (betterlockscreen uses it internally)
    programs.i3lock = {
      enable = true;
      package = pkgs.i3lock-color;
    };

    # xss-lock for locking on manual suspend
    programs.xss-lock = {
      enable = true;
      lockerCommand = "${lockScript}/bin/my-screenlock";
    };

    environment.systemPackages = with pkgs; [
      betterlockscreen
      xidlehook
      lockScript
    ];

    # Home Manager: xidlehook service for idle timeout
    nerv.home.extraOptions.services.xidlehook = {
      enable = true;
      not-when-fullscreen = true;
      not-when-audio = true;
      detect-sleep = true;
      timers = [
        # Notification before lock
        {
          delay = (cfg.inactiveInterval - cfg.notifyInterval) * 60;
          command = ''${pkgs.libnotify}/bin/notify-send -u critical -a "Screen Lock" "Locking in ${toString cfg.notifyInterval} minute(s)"'';
          canceller = ''${pkgs.libnotify}/bin/notify-send -a "Screen Lock" "Lock cancelled"'';
        }
        # Lock screen
        {
          delay = cfg.notifyInterval * 60;
          command = "${lockScript}/bin/my-screenlock";
        }
      ] ++ optionals (cfg.suspendInterval > 0) [
        # Notification before suspend
        {
          delay = (cfg.suspendInterval - cfg.notifyInterval) * 60;
          command = ''${pkgs.libnotify}/bin/notify-send -u critical -a "Screen Lock" "Suspending in ${toString cfg.notifyInterval} minute(s)"'';
          canceller = ''${pkgs.libnotify}/bin/notify-send -a "Screen Lock" "Suspend cancelled"'';
        }
        # Suspend system
        {
          delay = cfg.notifyInterval * 60;
          command = "systemctl suspend";
        }
      ];
    };

    # Update wallpaper cache on home-manager activation if wallpaper is specified
    nerv.home.activation.betterlockscreen-wallpaper = mkIf (cfg.wallpaper != null) (let
      cacheDir = "$HOME/.cache/betterlockscreen";
      cacheHashFile = "${cacheDir}/.cache-hash";
      wallpaperPath = toString cfg.wallpaper;
      cacheHash = "${wallpaperPath}:${cfg.effect}";
    in ''
      echo "[Screenlock] Checking betterlockscreen wallpaper cache..."
      mkdir -p ${cacheDir}
      if [ ! -f ${cacheHashFile} ] || [ "$(cat ${cacheHashFile} 2>/dev/null)" != "${cacheHash}" ]; then
        echo "[Screenlock] Updating wallpaper cache..."
        if PATH="${pkgs.xorg.xrdb}/bin:$PATH" ${pkgs.betterlockscreen}/bin/betterlockscreen -u "${wallpaperPath}" --fx ${cfg.effect}; then
          echo "${cacheHash}" > ${cacheHashFile}
          echo "[Screenlock] Wallpaper cache updated"
        else
          echo "[Screenlock] WARNING: Failed to update wallpaper cache"
        fi
      else
        echo "[Screenlock] Wallpaper cache is up to date"
      fi
    '');
  };
}
