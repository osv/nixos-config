{ options, config, pkgs, lib, inputs, ... }:

with lib;
with lib.nerv; {
  imports = [ inputs.sops-nix.nixosModules.sops ];

  config = {
    nerv.opt.persist.state.homeDirectories = [ ".config/sops" ];
    sops = {
      defaultSopsFile = ../../../../../secrets/secrets.yaml;
      secrets."users/userPassword".neededForUsers = true;
      # I think it will be easier to install system if I can use file
      # The persisted ~/.config isn't mounted fast enough
      age.keyFile = "/persist/state/home/${config.nerv.opt.user.name}/.config/sops/age/keys.txt";
    };

    sops.secrets = {
      vpn-kzd = { sopsFile = ../../../../../secrets/vpn.yaml; };
      vpn-kzp = { sopsFile = ../../../../../secrets/vpn.yaml; };
    };
  };
}
