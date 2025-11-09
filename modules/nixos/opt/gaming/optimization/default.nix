{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.gaming.optimization;
in
{
  options.nerv.opt.gaming.optimization = with types; {
    enable = mkBoolOpt false "Enable optimization tools related for gaming.";
    mangohud.enable = mkBoolOpt false "A Vulkan and OpenGL overlay for monitoring FPS.";
    gamemode.enable = mkBoolOpt false "Feral GameMode.";
    gamescope.enable = mkBoolOpt false "Valve Gamescope.";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment = {
        variables = {
          # Game perfomance can be increased, example:
          # LD_PRELOAD=$MIMALLOC_PATH MIMALLOC_LARGE_OS_PAGES=1 MIMALLOC_PURGE_DELAY=300
          MIMALLOC_PATH="${pkgs.mimalloc}/lib/libmimalloc.so";
        };
      };
    })

    # (mkIf (cfg.enable && cfg.gamescope.enable) {
    #   gamescope = {
    #     enable = yes;
    #     gamescope = yes;
    #   };
    #   hardware.opengl.extraPackages = [ pkgs.gamescope ];
    # })

    (mkIf (cfg.enable && cfg.mangohud.enable) {
      nerv.home.extraOptions.programs.mangohud = {
        enable = yes;
        settings = {
          fps = yes;
          frame_timing = yes;
          gpu_stats = yes;
          gpu_temp = yes;
          output_folder = "/tmp/";
          cpu_stats = yes;
          core_load = yes;
          cpu_temp = yes;
          ram = yes;
          vram = yes;
        };
      };
    })

    (mkIf (cfg.enable && cfg.gamemode.enable) {
      nerv.home.configFile."gamemode.ini".text = generators.toINI {} {
        general = {
          softrealtime = "auto";
          renice=16;
        };
        cpu = {
          park_cores="no";
          pin_cores="yes";
        };
        custom = {
          start = "${pkgs.libnotify}/bin/notify-send 'GameMode started'";
          end = "${pkgs.libnotify}/bin/notify-send 'GameMode ended'";
        };
      };

      programs.gamemode.enable = yes;
      programs.gamemode.enableRenice = yes;
    })


  ];
}
