{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.programming.ruby;
in
{
  options.nerv.opt.programming.ruby = with types; {
    enable = mkBoolOpt false "Whether or not to install and configure Ruby related stuff";
    rails-mcp-server = {
      enable = mkBoolOpt true "Whether or not to install rails-mcp-server";
      pkg = mkOpt package pkgs.nerv.rails-mcp-server "The rails-mcp-server package to use";
    };
  };

  config = mkIf cfg.enable {
    nerv.opt.common.manuals.files = [ ./pry_cheatsheet.md ];

    environment.systemPackages = with pkgs;
      [ ] ++ (lib.optional cfg.rails-mcp-server.enable cfg.rails-mcp-server.pkg);

    nerv.opt.persist.state = {

      # Today I'm using direnv, but need some persistent directories
      homeDirectories = [
        ".gem"
        ".bundle"
        ".config/rails-mcp"
      ];

      homeFiles = [ ".local/share/pry/pry_history" ];
    };
  };
}
