{
  description = "OSV Config";

  inputs = {
    # NixPkgs (nixos-25.11)
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

    # NixPkgs Unstable (nixos-unstable)
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nix-flatpak.url = "github:gmodena/nix-flatpak";

    claude-desktop.url = "github:k3d3/claude-desktop-linux-flake";
    claude-desktop.inputs.nixpkgs.follows = "nixpkgs";
    claude-desktop.inputs.flake-utils.follows = "flake-utils";

    stevenBlackHosts.url = "github:StevenBlack/hosts";

    # Nice zoom aplication (waiting untill upstream will include flake)
    boomer.url = "github:nilp0inter/boomer/971a082";

    # Generate System Images
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";

    # Snowfall Lib
    snowfall-lib.url = "github:snowfallorg/lib";
    snowfall-lib.inputs.nixpkgs.follows = "nixpkgs";

    # Comma
    comma.url = "github:nix-community/comma";
    comma.inputs.nixpkgs.follows = "unstable";

    # System Deployment
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "unstable";

    # Run unpatched dynamically compiled binaries
    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "unstable";

    nur.url = "github:nix-community/NUR";

    # Discord Replugged
    replugged.url = "github:LunNova/replugged-nix-flake";
    replugged.inputs.nixpkgs.follows = "unstable";

    pass-zsh-completion = {
      url = "github:ninrod/pass-zsh-completion";
      flake = false;
    };


    wallpapers-new = {
      url = "github:osv/wallpapers-new";
      flake = false;
    };
  };

  outputs = inputs:
    let
      lib = inputs.snowfall-lib.mkLib {
        inherit inputs;
        src = ./.;

        snowfall = {
          meta = {
            name = "nerv";
            title = "OSV Private CFG";
          };

          namespace = "nerv";
        };
      };
    in lib.mkFlake {
      channels-config.allowUnfree = true;
      channels-config.permittedInsecurePackages = [
        # "openssl-1.1.1w"
        # "nix-2.15.3"
      ];

      overlays = with inputs; [
        nur.overlays.default
      ];

      systems.modules = with inputs; [
        home-manager.nixosModules.home-manager
        inputs.nix-flatpak.nixosModules.nix-flatpak
        nix-ld.nixosModules.nix-ld
        nur.nixosModules.nur
        # sops-nix.nixosModules.sops
      ];

      homes.modules = with inputs; [
        nix-flatpak.homeManagerModules.nix-flatpak
      ];

      systems.hosts.xenon.modules = with inputs; [
        disko.nixosModules.disko
      ];
      systems.hosts.argon.modules = with inputs; [
        disko.nixosModules.disko
      ];
      systems.hosts.krypton.modules = with inputs; [
        disko.nixosModules.disko
      ];

      deploy = lib.mkDeploy { inherit (inputs) self; };

      checks = builtins.mapAttrs
        (system: deploy-lib: deploy-lib.deployChecks inputs.self.deploy)
        inputs.deploy-rs.lib;
    };
}
