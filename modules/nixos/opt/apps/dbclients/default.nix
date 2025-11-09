{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.dbclients;

  dbScripts = [
    [ "my-mysql-local"              pkgs.mycli          "mysql/local" ]
    [ "my-mysql-kz-feature2"        pkgs.mycli          "mysql/kz/feature2" ]
    [ "my-mysql-kz-reporting"       pkgs.mycli          "mysql/kz/reporting" ]
    [ "my-clickhouse-kz-reporting"  pkgs.clickhouse-cli "clickhouse/reporting" ]
    [ "my-clickhouse-kz-impression" pkgs.clickhouse-cli "clickhouse/impression" ]
  ];

  mkDBcli = script: pkgcli: pass: pkgs.writeShellScriptBin script ''
    echo 'Using pass: ${pass}'
    ${lib.getExe pkgcli} $(${lib.getExe pkgs.pass} ${pass})
  '';

  dbClientScripts = map (s: mkDBcli (elemAt s 0) (elemAt s 1) (elemAt s 2)) dbScripts;

in
{
  options.nerv.opt.apps.dbclients = with types; {
    enable = mkBoolOpt false "Whether or not to enable database clients.";
    mysql-workbench = mkBoolOpt false "Whether or not to setup mysql-workbench.";
    mycli = mkBoolOpt false "Whether or not to enable mysql console client and some connections scripts.";
    clickhouse-cli = mkBoolOpt false "Whether or not to enable Clickhouse console client and some connections scripts.";
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.mysql-workbench) {
      environment.systemPackages = with pkgs; [ mysql-workbench ];

      nerv = {
        opt.desktop.addons.keyring = on; # workbench requires keyring
        opt.persist.state.homeDirectories = [ ".mysql/workbench" ];
      };
    })

    (mkIf (cfg.enable && cfg.mycli) {
      environment.systemPackages = with pkgs; [
        mycli
      ] ++ filter (s: hasPrefix "my-mysql" s.name) dbClientScripts;
    })

    (mkIf (cfg.enable && cfg.clickhouse-cli) {
      environment.systemPackages = with pkgs; [
        clickhouse-cli
      ] ++ filter (s: hasPrefix "my-clickhouse" s.name) dbClientScripts; # Add clickhouse-specific scripts
    })
  ];
}
