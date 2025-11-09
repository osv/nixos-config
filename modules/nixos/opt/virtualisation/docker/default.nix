{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.virtualisation.docker;
  # Assumed dicker data directory is not changes
  isZfsDockerDir = config? fileSystems && config.fileSystems ? "/var/lib/docker" && config.fileSystems."/var/lib/docker".fsType == "zfs";

  my-docker-login-personal = pkgs.writeShellScriptBin "my-docker-login-personal" ''
    # Docker login using `pass` password manager
    set -x
    PASS_KEY=login.docker.com/personal
    LOGIN=$(${lib.getExe pkgs.pass} "$PASS_KEY" | grep login | sed 's/[^:]*: *//')
    ${lib.getExe pkgs.pass} "$PASS_KEY" | head -1 | docker login --username "$LOGIN" --password-stdin
  '';
in
{
  options.nerv.opt.virtualisation.docker = with types; {
    enable = mkBoolOpt false "Whether or not to enable Docker.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ docker-compose my-docker-login-personal ];
    nerv.opt.user.extraGroups = [ "docker" ];

    virtualisation.docker = on // (optionalAttrs isZfsDockerDir {
      storageDriver = "zfs";
    });
  };
}
