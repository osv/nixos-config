{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.system.xkb;
in
{
  options.nerv.opt.system.xkb = with types; {
    enable = mkBoolOpt false "Whether or not to configure xkb.";
    xkbPerWindow = mkBoolOpt false "Whether or not to use per window kayboard layout (KBDD).";
  };

  config = mkIf cfg.enable {
    console.useXkbConfig = true;
    services.xserver = {
      enableCtrlAltBackspace = true;
      xkb = {
        layout = "us,ru,ua";
        variant = "";
        options = "grp:rctrl_toggle, altwin:swap_alt_win, terminate:ctrl_alt_bksp";
      };
      autoRepeatDelay = 220; #  time of key must be depressed before autorepeat starts
      autoRepeatInterval = 25;
    };

    nerv.home.extraOptions.systemd.user.services.kbdd = optionalAttrs cfg.xkbPerWindow {
      Unit.Description = "Keyboard layout per window";
      Unit.PartOf = [ "graphical-session.target" ];
      Unit.After = [ "graphical-session-pre.target" ];
      # Service.Type = "exec";
      Service.ExecStart = "${pkgs.kbdd}/bin/kbdd -n";
      Install.WantedBy = [ "graphical-session.target" ];


# [Unit]
# Description=Xorg - Layout PerWindow - %P
# # Requisite=xorg.target
# PartOf=graphical-session.target
# # After=xorg.target wm.target
# # PartOf=main.target
# # Before=main.target

# [Service]
# ExecStartPre=/bin/sh -c 'pkill -x kbdd; true'
# ExecStart=/usr/bin/kbdd --nodaemon
# # ExecStop=/usr/bin/pkill -x kbdd
# Restart=always
# RestartSec=1
# TimeoutStopSec=1
# Environment=DISPLAY=:0

# [Install]
# # Alias=dbus-ru.gentoo.KbddService.service
# # WantedBy=main.target
# WantedBy=default.target
# WantedBy=suspend.target
# WantedBy=hibernate.target
# WantedBy=hybrid-sleep.target
    };
  };
}
