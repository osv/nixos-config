{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.gaming.steam;
  user = config.nerv.opt.user;
  home = config.users.users.${user.name}.home;
in
{
  options.nerv.opt.gaming.steam = with types; {
    enable = mkBoolOpt false "Whether or not to enable support for Steam.";
    enableNative = mkBoolOpt false "Native Steam";
    enableSteamBeta = mkBoolOpt false "Beta?";
    enableFlatpak = mkBoolOpt false "flatpak version of steam";
  };

  config = mkIf cfg.enable {
    # alias steam='protonup && setsid steam'
    # programs.steam = {
    #   enable = true;
    #   gamescopeSession.enable = true;
    #   extraCompatPackages = [ proton-ge-bin ];
    # };
    programs.steam = mkIf cfg.enableNative {
      enable = true;
      remotePlay.openFirewall = true;
      # extraCompatPackages = with pkgs; [
      #   luxtorpeda
      #   inputs.nix-proton-cachyos.packages.${system}.proton-cachyos
      #   inputs.chaotic.packages.${system}.proton-ge-custom
      # ];
      # gamescopeSession.enable = true;
      localNetworkGameTransfers.openFirewall = true;
      # protontricks.enable = true;
    };

    nerv.home = {
      file = {
        steam-beta = {
          enable = cfg.enableSteamBeta;
          text = "publicbeta";
          target = ".local/share/Steam/package/beta";
        };
      };
    };

    # # Steam hardware such as the Steam Controller, other supported controllers and the HTC Vive
    # hardware.steam-hardware.enable = true;
    # # Enable GameCube controller support.
    # services.udev.packages = [ pkgs.dolphinEmu ];

    # environment.systemPackages = with pkgs.nerv; [
    #   steam
    # ];

    # environment.sessionVariables = {
    #   STEAM_EXTRA_COMPAT_TOOLS_PATHS = "$HOME/.steam/root/compatibilitytools.d";
    # };

    nerv.opt.persist.state.homeDirectories = [
      ".local/share/Steam"
      ".steam"

      # games
      ".config/EgoSoft"
    ];

    nerv.home.extraOptions.services.flatpak = mkIf cfg.enableFlatpak {
      overrides = {
        "com.valvesoftware.Steam" = {
          Context = {
            filesystems = [
              "${home}/Games"
              "${home}/.local/share/applications"
              "${home}/.local/share/games"
              "${home}/.local/share/Steam"
            ];
          };
          # Environment = {
          #   PULSE_SINK = "Game";
          # };
          # "Session Bus Policy" = {
          #   org.freedesktop.Flatpak = "talk";
          # };
        };
      };
      packages = [
        "com.valvesoftware.Steam"
      ];
    };
  };
}
