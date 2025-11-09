{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.tools.misc;
in
{
  options.nerv.opt.tools.misc = with types; {
    enable = mkBoolOpt false "Whether or not to enable common utilities.";
  };

  config = mkIf cfg.enable {
    nerv.home = {
      configFile."wgetrc".text = "";
      # Script for download nix config files repos. I'm using them for searching interesting ideas
      # E.g. "rg --glob '!.git' --no-heading --line-number neededForBoot"
      file."work/other/nix-repos/download-nix-repos.sh".source = ./download-nix-repos.sh;
    };

    environment.systemPackages = with pkgs; [
      nerv.my-scripts
      nerv.myip
      fzf
      killall
      unzip
      file
      jq
      clac
      wget
      moreutils # sponge, pee (like tee but process)

      # Networking tools
      inetutils # hostname ping ifconfig...
      dnsutils # dig nslookup...
    ];
  };
}
