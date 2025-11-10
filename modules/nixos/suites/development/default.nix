{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.development;
  apps = {
    vscode = on;
    emacs = {
      enable = yes;
      defaultEditor = yes;
      doom = on;
    };
    dbclients = on // {
      mysql-workbench = yes;
      mycli = yes;
      clickhouse-cli = yes;
    };
    ai = on // { whisper = no; };
  };
  cli-apps = {
    tmux = on;
    prisma = on;
    bpftrace = on;
  };
in
{
  options.nerv.suites.development = with types; {
    enable = mkBoolOpt false
      "Whether or not to enable common development configuration.";
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 12345 3000 3001 8080 8081 ];

    nerv = {
      opt.apps = apps;
      opt.cli-apps = cli-apps;

      opt.tools = {
        direnv = on;
        go = on;
        http = on;
        k8s = on;
        # qmk = on;
      };

      opt.programming = {
        node = on;
        python = on;
        ruby = on;
      };

      opt.virtualisation = { docker = on; };
    };
  };
}
