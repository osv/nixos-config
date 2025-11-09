{ config, lib, pkgs, ... }:
# with pkgs.commonutils;
with lib;
with lib.nerv;

# based on https://github.com/wiedzmin/nixos-config.git
let cfg = config.nerv.opt.system.performance;
in {
  options.nerv.opt.system.performance = {
    enable = mkBoolOpt false "Whether to enable performance customizations.";
    makeLinuxFastAgain.enable = mkBoolOpt false
      "Whether add enable kernel optimizations (may leak your data!!!).";
    noWatchDog.enable = mkBoolOpt false
      "No watchdog. Do not enable on server conf. See https://wiki.archlinux.org/title/Improving_performance#Watchdogs";
    tweeks.enable = mkBoolOpt true
      "Enable CachyOS and other tweeks";
    appsSuspension.rules = mkOpt types.attrs { } "Apps suspending rules.";
    warmup.paths = mkOpt (types.listOf types.str) [ ] "Paths to pull.";
    oom.enable = mkBoolOpt false "Whether to enable userspace OOM solution(s)";
    oom.notifications = mkBoolOpt true ''
      Whether to enable early OOM notifications.
      Warning: enabling this option (while convenient) should not be done on a machine where you do not trust the other users as it allows any other local user to DoS your session by spamming notifications.
    '';
  };

  # TODO: Check this pls, cfg focused on gaming https://github.com/keenanweaver/nix-config/blob/main/modules/components/performance/default.nix
  config = mkMerge [
    (mkIf (cfg.enable && cfg.noWatchDog.enable) {
      # https://wiki.archlinux.org/title/improving_performance#Watchdogs
      boot = {
        kernelParams = [ "nowatchdog" "kernel.nmi_watchdog=0" ];
        blacklistedKernelModules = [
          "sp5100_tco" # AMD
          "iTCO_wdt" # Intell
        ];
      };
    })

    # Required for OOM
    (mkIf (cfg.enable && cfg.oom.enable && cfg.oom.notifications) {
      services.systembus-notify.enable = true;
    })

    (mkIf (cfg.enable && cfg.tweeks.enable) {
      # https://github.com/keenanweaver/nix-config/blob/main/modules/components/performance/default.nix
      # see also https://github.com/CachyOS/CachyOS-Settings/blob/master/etc/sysctl.d/99-cachyos-settings.conf
      boot = {
        kernel.sysctl = {
          # The kernel flusher threads will periodically wake up and write old data out to disk.  This
          # tunable expresses the interval between those wakeups, in 100'ths of a second (Default is 500).
          "vm.dirty_writeback_centisecs" = 1500;

          # Increase netdev receive queue
          # May help prevent losing packets (def. 1000)
          "net.core.netdev_max_backlog" = 16384;

          # Increase the maximum connections
          # The upper limit on how many connections the kernel will accept (default 4096 since kernel version 5.6):
          "net.core.somaxconn" = 8192;

          # Disable TCP slow start after idle
          # Helps kill persistent single connection performance
          "net.ipv4.tcp_slow_start_after_idle" = 0;

          # Enable TCP Fast Open
          # TCP Fast Open is an extension to the transmission control protocol (TCP) that helps reduce network latency
          # by enabling data to be exchanged during the sender’s initial TCP SYN [3].
          # Using the value 3 instead of the default 1 allows TCP Fast Open for both incoming and outgoing connections:
          "net.ipv4.tcp_fastopen" = 3;

          # Enable BBR3
          # The BBR3 congestion control algorithm can help achieve higher bandwidths and lower latencies for internet traffic
          "net.ipv4.tcp_congestion_control" = "bbr";
        };
      };
      security = {
        pam = {
          loginLimits = [
            # https://scribe.rip/@a.b.t./here-are-some-possibly-useful-tweaks-for-steamos-on-the-steam-deck-fcb6b571b577
            # https://github.com/RPCS3/rpcs3/issues/9328#issuecomment-732390362
            # https://github.com/CachyOS/CachyOS-Settings/tree/master/etc/security/limits.d
            { domain = "*"; item = "nofile"; type = "-"; value = "unlimited"; }
            { domain = "*"; item = "memlock"; type = "-"; value = "unlimited"; } # RPCS3
            { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; } # Realtime audio
            { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
            { domain = "@realtime"; item = "memlock"; type = "-"; value = "unlimited"; }
            { domain = "@realtime"; item = "rtprio"; type = "-"; value = "99"; }
          ];
        };
      };
    })

    (mkIf (cfg.enable && cfg.makeLinuxFastAgain.enable) {
      boot = {
        kernelParams = optionals cfg.makeLinuxFastAgain.enable [
          "tsx=on" # Enables Intel Transactional Synchronization Extensions (TSX), which can improve performance for certain workloads that use transactional memory.
          "mitigations=off" # Disables all security mitigations. This can significantly improve performance, but it can also make the system very vulnerable to security attacks.
        ];
      };
    })

    (mkIf cfg.enable {
      boot = {
        kernelModules = [ "bfq" ];
      };

      services = {
        irqbalance.enable = true; # Should improve latency, also see /proc/interrupts
        earlyoom = {
          enable = cfg.oom.enable;
          freeMemThreshold = 4; # Default 10 is too big
          enableNotifications = cfg.oom.notifications;
          extraArgs = [
            "--avoid" "'(^|/)(init|Xorg|ssh)$'"
            "--prefer" "'(^|/)(node|java|chromium|.?firefox|ruby)$'"
          ];
        };

        journald = {
          extraConfig = ''
          SystemMaxUse=100M
        '';
        };

        udev = {
          extraRules = ''
            # Realtime Audio (https://gentoostudio.org/?page_id=420)
            KERNEL=="rtc0", GROUP="audio"
            KERNEL=="hpet", GROUP="audio"
          '';
        };
      };

      nerv.home.extraOptions = {
        services.xsuspender = optionalAttrs (cfg.appsSuspension.rules != { }) {
          enable = true;
          defaults = {
            suspendDelay = 10;
            onlyOnBattery = false;
          };
          inherit (cfg.appsSuspension) rules;
        };
      };

      systemd.user.services = optionalAttrs (cfg.warmup.paths != [ ]) {
        # systemctl --user restart xsuspender.service
        # systemctl --user stop xsuspender.service
        "warmup" = {
          description = "Warm up paths";
          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${pkgs.vmtouch}/bin/vmtouch -t ${
                lib.concatStringsSep " " cfg.warmup.paths
              }";
            StandardOutput = "journal";
            StandardError = "journal";
          };
          after = [ "graphical-session-pre.target" ];
          partOf = [ "graphical-session.target" ];
          wantedBy = [ "graphical-session.target" ];
        };
      };
    })
  ];
}
