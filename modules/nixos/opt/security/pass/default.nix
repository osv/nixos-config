{ options, config, pkgs, lib, inputs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.security.pass;

in
{
  options.nerv.opt.security.pass = with types; {
    enable = mkBoolOpt false "Whether or not to enable PASS.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # order is important?
      (pass.withExtensions (ext: with ext; [pass-import pass-checkup]))
      pass
    ];

    nerv = {
      home.extraOptions.programs = {
        zsh = {
          plugins = [
            {
              name = "pass-zsh-completion";
              file = "pass-zsh-completion.plugin.zsh";
              src = inputs.pass-zsh-completion;
            }
          ];
        };
        browserpass = {
          enable = true;
          browsers = [ "firefox" "chromium" ];
        };
      };
      
      opt.persist.state.homeDirectories = [{
        directory = ".password-store";
        mode = "0700";
      }];
    };
  };
}
