{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.apps.torrent;
in
{
  options.nerv.opt.apps.torrent = with types; {
    enable = mkBoolOpt false "Whether or not to enable Torrent client.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ qbittorrent ];

    # You can check for open port in logs: ~/.local/share/qBittorrent/logs/qbittorrent.log
    networking.firewall.allowedTCPPorts = [ 34915 ];
    networking.firewall.allowedUDPPorts = [ 34915 ];

    nerv.opt.persist.state.homeDirectories = [
      ".config/qBittorrent"
      ".local/share/qBittorrent" # logs, geodb cache
    ];
  };
}
