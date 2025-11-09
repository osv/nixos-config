{ channels, ... }:

final: prev: {
  nix-output-monitor = prev.nix-output-monitor.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      # Prefer icons from nerd fonts
      # ./01-nom-nerd-font.diff
    ];
  });
}
