{ options, config, pkgs, lib, inputs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.home;
in
{
  imports = with inputs; [ home-manager.nixosModules.home-manager ];

  options.nerv.home = with types; {
    file = mkOpt attrs { }
      "A set of files to be managed by home-manager's <option>home.file</option>.";
    configFile = mkOpt attrs { }
      "A set of files to be managed by home-manager's <option>xdg.configFile</option>.";
    activation = mkOpt attrs { }
      "A set of activation scripts to be managed by home-manager's <option>home.activation</option>.";
    extraOptions = mkOpt attrs { } "Options to pass directly to home-manager.";
  };

  config = {
    # Would like to run some home-manager commands sometimes
    environment.systemPackages = with pkgs; [ home-manager ];

    nerv.home.extraOptions = {
      home.stateVersion = config.system.stateVersion;
      home.file = mkAliasDefinitions options.nerv.home.file;
      home.activation = mkAliasDefinitions options.nerv.home.activation;
      xdg = on // {
        configFile = mkAliasDefinitions options.nerv.home.configFile;
        # TODO: Configure userDirs
        userDirs = on;

      };
    };

    nerv.opt.persist.state.homeDirectories = [
      # std xdg dirs
      "Documents"
      "Downloads"
      "Music"
      "Pictures"
      "Public"
      "Templates"
      "Videos"
      "Desktop"

      "torrents"
      # my space
      "work"
    ];

    home-manager = {
      useUserPackages = true;

      users.${config.nerv.opt.user.name} =
        mkAliasDefinitions options.nerv.home.extraOptions;
    };
  };
}
