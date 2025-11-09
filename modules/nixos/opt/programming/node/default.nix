{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.programming.node;
in
{
  options.nerv.opt.programming.node = with types; {
    enable = mkBoolOpt false "Whether or not to install and configure NodeJS";
    pkg = mkOpt package pkgs.nodejs_22 "The NodeJS package to use";
    prettier = {
      enable = mkBoolOpt true "Whether or not to install Prettier";
      pkg =
        mkOpt package pkgs.nodePackages.prettier "The NodeJS package to use";
    };
    yarn = {
      enable = mkBoolOpt true "Whether or not to install Yarn";
      pkg = mkOpt package pkgs.nodePackages.yarn "The NodeJS package to use";
    };
    flyctl = {
      enable = mkBoolOpt true "Whether or not to install flyctl";
      pkg = mkOpt package pkgs.flyctl "The flyctl package to use";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ cfg.pkg ] ++ (lib.optional cfg.prettier.enable cfg.prettier.pkg)
      ++ (lib.optional cfg.yarn.enable cfg.yarn.pkg)
      ++ (lib.optional cfg.flyctl.enable cfg.flyctl.pkg);

    nerv.home.extraOptions = {
      home.sessionPath = [ "$HOME/.npm-global/bin" ];
    };

    environment.variables = {
      NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config"; # default ~/.npmrc
      NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
      # NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
      NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      NODE_REPL_HISTORY = "$XDG_DATA_HOME/node/repl_history";
    };

    nerv.opt.persist.state = {
      homeDirectories = [
        ".npm-global"
      ];
      homeFiles = [
        ".config/npm/config"
        ".local/share/node/repl_history"
      ];
    };
  };
}
