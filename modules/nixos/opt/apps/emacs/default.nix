{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.emacs;
  doomCfg = cfg.doom;

  targetEmacs = pkgs.emacs30;
  emacsWithPackages = (pkgs.emacsPackagesFor targetEmacs).emacsWithPackages;

  # Note from
  # Empty package list as I use straight.el now to try it out.
  # vterm is an exception as it does not currently build in a
  # naive way, which makes straight.el support difficult.
  emacsPkg = emacsWithPackages (epkgs: [ epkgs.vterm ]);

  # Next packages for emacs only, lets make system more pure
  emacsEnvPackages = with pkgs;
    [ ]
    ++ optionals (cfg.env.aiCodeium) [
      codeium
    ]
    ++ optionals (cfg.env.aiCopilot) [
      copilot-language-server
    ]
    ++ optionals (cfg.env.stdDoom) [
      # Programs needed at runtime or for straight to build packages
      # binutils
      # bash
      cmake
      gnumake
      gcc
      libtool
      # :tools editorconfig
      editorconfig-core-c # # :tools editorconfig - per-project style config
      zstd # :emacs undo
    ]
    ++ optionals (cfg.env.doomOPtional) [
      fd # faster projectile indexing
      imagemagick # for image-dired
      zstd # for undo-fu-session/undo-tree compression
      sqlite # :lang (org +roam2) :tools lookup
      unzip # :tools debugger
      pre-commit # :tools magit
      graphviz # :lang (org +roam2) :lang plantuml
      jq # rest +jq
      emacs-lsp-booster # lsp
      nodePackages.sql-formatter
    ]
    ++ optionals (cfg.env.spell) [
      (aspellWithDicts (p:
        with p; [
          en
          ru
          en-computers
          en-science
        ])) # :checkers (spell +aspell)
      cmigemo # :lang japanese
      wordnet # :tools (lookup +dictionary +offline) -- wordnut-search
    ]
    ++ optionals (cfg.env.langNix) [
      # rnix-lsp # :lang (nix +lsp)
      # nixfmt # :lang nix :editor format
    ]
    ++ optionals (cfg.env.langWeb) [
      nodePackages.js-beautify # :lang web
      html-tidy # :lang web
    ]
    ++ optionals (cfg.env.haskell) [
      haskellPackages.cabal-fmt # :lang haskell :editor format
      haskellPackages.cabal-install # :lang haskell
      haskellPackages.haskell-language-server # :lang (haskell +lsp)
      haskellPackages.hoogle # :lang haskell
      haskellPackages.ormolu # :lang haskell :editor format
    ]
    ++ optionals (cfg.env.langSh) [
      shellcheck # :lang sh
      shfmt # :lang sh :editor format
    ]
    ++ optionals (cfg.env.langPython) [
      python3Packages.black # :lang python :editor format
      python3Packages.isort # :lang python
      python3Packages.pyflakes # :lang python
      python3Packages.python-lsp-server # :lang python (+lsp)
      python3
    ]
    ++ optionals (cfg.env.langXml) [
      # TODO: Check saxon and emacs xpath module for it
      openjdk17-bootstrap # xml-lsp
      libxml2 # :lang data
    ]
    ++ optionals (cfg.env.nodejs) [
      nodejs # :tools debugger
    ]
    ++ optionals (cfg.env.langJS) [
      #nodePackages.eslint # :lang (json +lsp)
      #nodePackages.stylelint # :lang web
      nodePackages.js-beautify # :lang web
      nodePackages.prettier # :editor format
    ]
    ++ optionals (cfg.env.langCPP) [
      clang-tools # :lang (cc +lsp) :editor format
    ]
    ++ optionals (cfg.env.nodejsLSP) [
      nodePackages.bash-language-server # :lang (sh +lsp)
      nodePackages.dockerfile-language-server-nodejs # :tools (docker +lsp)
      nodePackages.vscode-css-languageserver-bin # :lang (web +lsp)
      nodePackages.vscode-html-languageserver-bin # :lang (web +lsp)
      nodePackages.vscode-json-languageserver-bin # :lang (json +lsp)
      nodePackages.yaml-language-server # :lang (yaml +lsp)
    ]
    ++ optionals (cfg.env.latex) [
      texlive.combined.scheme-medium # latex preview in org-mode
    ]
    ++ optionals (cfg.env.export) [
      gnuplot # :lang (org +gnuplot)
      pandoc # :lang org markdown latex
      graphviz # plantuml
      python3Packages.grip # :lang (markdown +grip) - GitHub-flavored preview
    ];
  myEmacsPkg = emacsPkg // (pkgs.symlinkJoin {
    name = "my-emacs";
    paths = [ emacsPkg ];
    nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
    postBuild = ''
      wrapProgram $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath emacsEnvPackages} \
        --set LSP_USE_PLISTS true
      wrapProgram $out/bin/emacsclient \
        --prefix PATH : ${lib.makeBinPath emacsEnvPackages} \
        --set LSP_USE_PLISTS true
    '';
  });
in
{
  options.nerv.opt.apps.emacs = with types; {
    enable = mkBoolOpt false "Whether or not to enable Emacs.";

    defaultEditor = mkBoolOpt false "Whether or not to set Emacs as default editor.";

    emacsVersion = mkOption {
      type = types.enum [ "stable" "git" ];
      default = "stable";
    };

    doom = rec {
      enable = mkBoolOpt false "Whether or not to enable Doom Emacs";
      repoUrl = mkOpt' types.str "https://github.com/doomemacs/doomemacs";
    };

    env = rec {
      aiCodeium = mkBoolOpt false "Add codeium";
      aiCopilot = mkBoolOpt true "Add GitHub Copilot LSP server";
      stdDoom = mkBoolOpt true "Add packages to env used by doom emacs";
      doomOPtional = mkBoolOpt true "Optional packages";
      spell = mkBoolOpt true "Packages for spell check";
      langNix = mkBoolOpt true "Nix language support";
      langWeb = mkBoolOpt true "HTML";
      haskell = mkBoolOpt true "Haskell";
      langSh = mkBoolOpt true "Shell scripting";
      langPython = mkBoolOpt true "Python";
      langXml = mkBoolOpt true "XML";
      nodejs = mkBoolOpt true "nodejs";
      langJS = mkBoolOpt true "JS tools for web";
      langCPP = mkBoolOpt true "C/C++";
      nodejsLSP = mkBoolOpt false "Nodejs language servers";
      latex = mkBoolOpt true "Latex (preview in org mode too)";
      export = mkBoolOpt true "Markdown and other export tools";
    };
  };

  config = mkIf cfg.enable {
    fonts.packages = with pkgs; [
      emacs-all-the-icons-fonts # spacemacs
    ];

    nerv.home.extraOptions = {
      services.emacs = {
        enable = yes;
        package = myEmacsPkg;
        socketActivation.enable = yes;
        startWithUserSession = "graphical";
      };

      systemd.user.services.emacs = {
        Unit = {
          # Keep the PartOf but prevent restart on target changes
          X-RestartIfChanged = false;
          X-StopIfChanged = false;
        };
        Service = {
          # Don't kill when target restarts
          KillMode = "mixed";
          KillSignal = "USR2";  # Emacs handles this gracefully
          # Fast shutdown - don't wait 90s default timeout
          TimeoutStopSec = 10;
        };
      };

      home.packages = with pkgs; [
        (makeDesktopItem {
          name = "emacsclient";
          desktopName = "Emacs (Client)";
          genericName = "Text Editor";
          icon = "emacs";
          exec = "${myEmacsPkg}/bin/emacsclient -c -a ${myEmacsPkg}/bin/emacs %F";
          categories = [ "Development" "TextEditor" "Utility" ];
          mimeTypes = [
            "text/english"
            "text/plain"
            "text/x-makefile"
            "text/x-c++hdr"
            "text/x-c++src"
            "text/x-chdr"
            "text/x-csrc"
            "text/x-java"
            "text/x-moc"
            "text/x-pascal"
            "text/x-tcl"
            "text/x-tex"
            "text/x-python"
            "text/vbscript"
            "application/x-shellscript"
          ];
          terminal = false;
        })
      ];
    };

    environment = {
      variables = {
        EDITOR = mkIf cfg.defaultEditor (mkOverride 900 "${myEmacsPkg}/bin/emacsclient -c -nw -a ${myEmacsPkg}/bin/emacs");
        VISUAL = mkIf cfg.defaultEditor (mkOverride 900 "${myEmacsPkg}/bin/emacsclient -c -a ${myEmacsPkg}/bin/emacs");
      };
      systemPackages = with pkgs; [
        myEmacsPkg

        ## Doom dependencies
        git
        ripgrep # lookup
        gnutls # doom! for TLS connectivity
        sqlite # lookup
        wordnet # lookup

        ## Keybindings documentation
        pkgs.nerv.my-generate-keybindings-index

        ## Test config script
        (mkTestConfigScript pkgs {
          name = "my-test-emacs-config";
          appName = "Emacs Doom";
          sourcePath = "modules/nixos/opt/apps/emacs/config_doom";
          targetPath = ".config/doom";
          files = [
            "init.el"
            "config.el"
            "packages.el"
            "themes"
            "keybindings-export.el"
            { source = "lib/keybinding-template.html"; target = "keybinding-template.html"; }
          ];
          reloadCmd = "SPC h r r";
        })
      ]
      ++ optionals (cfg.env.export) [
        go-grip # :lang (markdown +grip) - GitHub-flavored preview
      ];
    };

    nerv.home.extraOptions = {
      home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];
    };

    nerv.home = {
      configFile = {
        "doom/init.el".source = ./config_doom/init.el;
        "doom/config.el".source = ./config_doom/config.el;
        "doom/packages.el".source = ./config_doom/packages.el;
        "doom/themes/doom-nano-light-theme.el".source = ./config_doom/themes/doom-nano-light-theme.el;
        "doom/themes/doom-nano-dark-theme.el".source = ./config_doom/themes/doom-nano-dark-theme.el;
        "doom/keybindings-export.el".source = ./config_doom/keybindings-export.el;
        "doom/keybinding-template.html".source = ../../../../../lib/keybinding-template.html;
      };
    };

    nerv.opt.persist = {
      state.homeDirectories = [
        "org"                    # org roam storage
        ".config/github-copilot" # copilot lsp
      ];
      derivative.homeDirectories = [
        ".config/emacs"
       # ".config/doom/snippets" # FIXME persist snippets, change directory?
      ];
    };

    nerv.home.activation = mkIf doomCfg.enable {
      # installDoomEmacs =
      #   lib.home-manager.hm.dag.entryAfter [ "writeBoundary" ] ''
      #     if [ ! -f "$XDG_CONFIG_HOME/emacs/.doomrc" ]; then
      #       echo "==> [Emacs] $XDG_CONFIG_HOME/emacs"
      #       set +e
      #       # Cloning to tmp directory, ~/.config/emacs might be mounted as persist dir
      #       ${lib.getExe pkgs.git} clone --depth=1 --single-branch "${doomCfg.repoUrl}" /tmp/doom_emacs
      #       cp -r /tmp/doom_emacs/. "$XDG_CONFIG_HOME/emacs/"
      #       rm -rf /tmp/doom_emacs
      #       set -e
      #     fi
      #   '';
    };
  };
}
