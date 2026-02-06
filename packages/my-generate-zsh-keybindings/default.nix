{ pkgs, lib, ... }:

let
  templatePath = ../../lib/keybinding-template.html;

  script = pkgs.replaceVars ./my-generate-zsh-keybindings.zsh {
    templatePath = templatePath;
    indexGenerator = lib.getExe pkgs.nerv.my-generate-keybindings-index;
    xdgOpen = "${pkgs.xdg-utils}/bin/xdg-open";
    zsh = lib.getExe pkgs.zsh;
  };
in
pkgs.stdenvNoCC.mkDerivation {
  name = "my-generate-zsh-keybindings";

  dontUnpack = true;

  installPhase = ''
    mkdir -p $out/bin
    cp ${script} $out/bin/my-generate-zsh-keybindings
    chmod +x $out/bin/my-generate-zsh-keybindings
  '';

  meta = {
    description = "Generate interactive HTML page with Zsh keybindings";
    mainProgram = "my-generate-zsh-keybindings";
  };
}
