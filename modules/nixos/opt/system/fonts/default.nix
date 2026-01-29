{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.system.fonts;

  my-test-powerline = pkgs.writeScriptBin "my-test-powerline"
    (builtins.readFile ./my-test-powerline.sh);
  my-test-fonts =
    pkgs.writeScriptBin "my-test-fonts" (builtins.readFile ./my-test-fonts.sh);
  my-test-charmap = pkgs.writeScriptBin "my-test-charmap" ''
    #!${pkgs.stdenv.shell}
    echo "Cozette Nerd Charmap"
    echo "https://raw.githubusercontent.com/slavfox/Cozette/master/img/charmap.txt"
    cat ${./charmap.txt}
  '';
in {
  options.nerv.opt.system.fonts = with types; {
    enable = mkBoolOpt false "Whether or not to manage fonts.";
    fonts = mkOpt (listOf package) [ ] "Custom font packages to install.";
  };

  config = mkIf cfg.enable {
    environment.variables = {
      # Enable icons in tooling since we have nerdfonts.
      LOG_ICONS = "true";
    };

    environment.systemPackages = with pkgs; [
      font-manager
      my-test-powerline
      my-test-fonts
      my-test-charmap
    ];

    fonts.packages = with pkgs;
      [
        terminus_font
        kochi-substitute-naga10 # urxvt Kochi Gothic

        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-cjk-serif
        noto-fonts-color-emoji
        source-code-pro
        fira-mono
        fira-code
        fira
        roboto-mono
        dejavu_fonts
        meslo-lgs-nf

        # emacs testing
        cascadia-code
        jetbrains-mono
        julia-mono
        roboto
        roboto-slab

        iosevka-comfy.comfy
        iosevka-comfy.comfy-duo
        iosevka-comfy.comfy-wide
        iosevka-comfy.comfy-fixed
        iosevka-comfy.comfy-motion
        iosevka-comfy.comfy-wide-duo
        iosevka-comfy.comfy-wide-fixed
        iosevka-comfy.comfy-motion-duo
        iosevka-comfy.comfy-wide-motion
        iosevka-comfy.comfy-motion-fixed
        iosevka-comfy.comfy-wide-motion-duo
        iosevka-comfy.comfy-wide-motion-fixed

        cozette
        spleen
        tamsyn
        tamzen
        symbola

        # Nerd Fonts (updated from nerdfonts.override)
        # nerd-fonts._3270
        # nerd-fonts.agave
        # nerd-fonts.anonymous-pro
        # nerd-fonts.arimo
        # nerd-fonts.aurulent-sans-mono
        # nerd-fonts.bigblue-terminal
        # nerd-fonts.bitstream-vera-sans-mono
        # nerd-fonts.cascadia-code
        # nerd-fonts.code-new-roman
        # nerd-fonts.comic-shanns-mono
        # nerd-fonts.cousine
        # nerd-fonts.daddy-time-mono
        # nerd-fonts.dejavu-sans-mono
        # nerd-fonts.droid-sans-mono
        # nerd-fonts.fantasque-sans-mono
        nerd-fonts.fira-code
        nerd-fonts.fira-mono
        # nerd-fonts.go-mono
        # nerd-fonts.gohu
        nerd-fonts.hack
        # nerd-fonts.hasklig
        # nerd-fonts.heavy-data
        # nerd-fonts.hermit
        # nerd-fonts.ia-writer
        # nerd-fonts.ibm-plex-mono
        # nerd-fonts.inconsolata
        # nerd-fonts.inconsolata-go
        # nerd-fonts.inconsolata-lgc
        nerd-fonts.iosevka
        nerd-fonts.iosevka-term
        nerd-fonts.jetbrains-mono
        # nerd-fonts.lekton
        # nerd-fonts.liberation-mono
        nerd-fonts.lilex
        # nerd-fonts.meslo
        # nerd-fonts.monofur
        # nerd-fonts.monoid
        # nerd-fonts.mononoki
        # nerd-fonts.mplus
        nerd-fonts.symbols-only
        # nerd-fonts.noto
        # nerd-fonts.open-dyslexic
        # nerd-fonts.overpass
        # nerd-fonts.pro-font
        # nerd-fonts.proggy-clean
        nerd-fonts.roboto-mono
        # nerd-fonts.share-tech-mono
        # nerd-fonts.source-code-pro
        # nerd-fonts.space-mono
        # nerd-fonts.terminus
        # nerd-fonts.tinos
        # nerd-fonts.ubuntu
        # nerd-fonts.ubuntu-mono
        # nerd-fonts.victor-mono

        # Custom fonts
        nerv.minimap-font
      ] ++ cfg.fonts;
  };
}
