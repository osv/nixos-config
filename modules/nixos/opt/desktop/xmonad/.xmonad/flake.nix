{
  description = "XMonad Config - uses nixpkgs from parent flake";

  inputs = {
    parent.url = "path:/home/osv/work/my/nixos-config";
  };

  outputs = inputs@{ self, parent, ... }:
    let
      system = "x86_64-linux";
      pkgs = parent.inputs.nixpkgs.legacyPackages.${system};

      haskellDeps = ps:
        with ps; [
          xmonad
          xmonad-contrib
          xmonad-extras
          ormolu
          cabal-fmt
          hoogle
          haskell-language-server
        ];
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          (ghc.withPackages haskellDeps)
        ];

        shellHook = ''
          echo "XMonad development environment"
          echo "GHC: ${pkgs.ghc.version}"
          echo "Nixpkgs: ${parent.inputs.nixpkgs.shortRev or "unknown"}"
          echo "Available: xmonad, ormolu, cabal-fmt, hoogle, hls"
        '';
      };
    };
}
