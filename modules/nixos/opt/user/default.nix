{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.user;
  defaultIconFileName = "profile.png";
  defaultIcon = pkgs.stdenvNoCC.mkDerivation {
    name = "default-icon";
    src = ./. + "/${defaultIconFileName}";

    dontUnpack = true;

    installPhase = ''
      cp $src $out
    '';

    passthru = { fileName = defaultIconFileName; };
  };
  propagatedIcon = pkgs.runCommand "propagated-icon" {
    passthru = { fileName = cfg.icon.fileName; };
  } ''
    local target="$out/share/nerv-icons/user/${cfg.name}"
    mkdir -p "$target"

    cp ${cfg.icon} "$target/${cfg.icon.fileName}"
  '';
in {
  options.nerv.opt.user = with types; {
    name = mkOpt str "osv" "The name to use for the user account.";
    fullName = mkOpt str "Olexandr Sydorchuk" "The full name of the user.";
    email = mkOpt str "olexandr.syd@gmail.com" "The email of the user.";
    icon = mkOpt (nullOr package) defaultIcon
      "The profile picture to use for the user.";
    extraGroups = mkOpt (listOf str) [ ] "Groups for the user to be assigned.";
    extraOptions = mkOpt attrs { }
      "Extra options passed to <option>users.users.<name></option>.";
  };

  config = {
    environment.systemPackages = with pkgs; [
      bat # colorized cat
      htop
      grc # colorize output of some commands, e.g "grc mount", need to source grc.zsh later
      cowsay
      fortune
      lolcat # omg, so sloooow, TODO: remove
      fzf
      propagatedIcon
      fd
      ripgrep
      tlrc # tldr - simplified pages with practical examples
      eza # exa replacement
      nerv.my-generate-zsh-keybindings
    ];
    environment.variables = {
      BAT_THEME = "base16"; # The theme is defined by the terminal's colors. URXVT has a setup for multiple themes, and this works well.
    };

    documentation.man.generateCaches = true; # check man in carapace
    programs.zsh = {
      enable = true;
      autosuggestions.enable = true;
      histFile = "$XDG_DATA_HOME/zsh/zsh_history";
    };

    nerv.opt.persist.state = { homeDirectories = [ ".local/share/zsh" ]; };
    nerv.opt.common.manuals.files = [ ./README.zsh.md ];
    nerv.home = {
      # TODO: Move these files
      file = {
        ".face".source = cfg.icon;
        ".face.icon".source = cfg.icon;
        "Pictures/${
          cfg.icon.fileName or (builtins.baseNameOf cfg.icon)
        }".source = cfg.icon;
      };

      configFile."starship.toml".source = ./starship.toml;

      extraOptions = {
        home.shellAliases = {
          lc = "${lib.getExe pkgs.colorls} --sd";
          lcg = "lc --gs";
          lcl = "lc -1";
          lclg = "lc -1 --gs";
          lcu = "${lib.getExe pkgs.colorls} -U";
          lclu = "lcu -U -1";

          exa = "${lib.getExe pkgs.eza} --group-directories-first --git";
          l = "exa -lbF --icons";
          ll = "exa -lbGF --icons";
          lla = "exa -labGF --icons";
          llm = "ll --sort=modified";
          la = "LC_COLLATE=C exa -ablF --icons";
          tree = "exa --tree";

          j = "journalctl --no-hostname -eb -o short-monotonic";
          jw = "j -p notice";
          jf = "j -f";
          jk = "j -k";
          jhome = "j -u home-manager-osv.service";
        };

        programs = {
          fzf = on // {
            defaultCommand = "fd --type f --hidden --follow --exclude .git";
            fileWidgetCommand = "fd --type f --hidden --follow --exclude .git"; # C-t
            changeDirWidgetCommand = "fd --type d --hidden --exclude .git"; # M-c
            changeDirWidgetOptions = [ # M-c
              "--preview '${pkgs.eza}/bin/eza --oneline --icons --color=always --group-directories-last --git --long {}'"
            ];
            fileWidgetOptions = [ "--preview '${pkgs.bat}/bin/bat --color=always --style=numbers --line-range :300 {}'" ]; # C-t
            defaultOptions = [ "--color=16" ]; # I want to use my term theme
          };
          starship = on;
          carapace = on;        # autocomplete
          zsh = {
            enable = true;
            enableCompletion = true;
            autosuggestion.enable = true;
            syntaxHighlighting.enable = true;
            history = {
              size = 30000;
              save = 30000;
              path = "$XDG_DATA_HOME/zsh/zsh_history";
              ignoreAllDups = true;
              ignoreSpace = true;
              expireDuplicatesFirst = true;
              extended = true;
              share = true;
            };

            initContent = ''
              # Fix an issue with tmux.
              export KEYTIMEOUT=1
              FZF_MARKS_FILE="$XDG_DATA_HOME/zsh/fzf-marks"
              # Use emacs bindings.
              set -o emacs

              fortune -s

              # Key bindings
              bindkey '^[[A' history-beginning-search-backward
              bindkey '^[[B' history-beginning-search-forward

              # Ctrl+X / - force file path completion (same as Emacs)
              zle -C complete-files complete-word _generic
              zstyle ':completion:complete-files:*' completer _files
              bindkey '^X/' complete-files

              key[Home]="''${terminfo[khome]}"
              key[End]="''${terminfo[kend]}"
              key[Insert]="''${terminfo[kich1]}"
              key[Backspace]="''${terminfo[kbs]}"
              key[Delete]="''${terminfo[kdch1]}"
              key[Up]="''${terminfo[kcuu1]}"
              key[Down]="''${terminfo[kcud1]}"
              key[Left]="''${terminfo[kcub1]}"
              key[Right]="''${terminfo[kcuf1]}"
              key[PageUp]="''${terminfo[kpp]}"
              key[PageDown]="''${terminfo[knp]}"
              key[Shift-Tab]="''${terminfo[kcbt]}"
              key[Control-Left]="''${terminfo[kLFT5]}"
              key[Control-Right]="''${terminfo[kRIT5]}"

              [[ -n "''${key[Control-Left]}"  ]] && bindkey -- "''${key[Control-Left]}"  backward-word
              [[ -n "''${key[Control-Right]}" ]] && bindkey -- "''${key[Control-Right]}" forward-word

              # bindkey "\e$terminfo[kcub1]" backward-word
              # bindkey "\e$terminfo[kcuf1]" forward-word
              # bindkey ";5C" forward-word
              # bindkey ";5D" backward-word

              # I like stop backward-kill-word on directory delimiter
              autoload -U select-word-style
              select-word-style bash

              # Create a reminder with human-readable durations, e.g. 15m, 1h, 40s, etc
              function r {
                local time=$1; shift
                sched "$time" "notify-send --urgency=critical 'Reminder' '$@'; ${pkgs.ding}";
              }; compdef r=sched

              # Export keybindings to HTML (Alt+Shift+F1)
              my-export-zsh-keybindings() {
                local tmpfile="''${XDG_RUNTIME_DIR:-/tmp}/zsh-bindkey-export"
                bindkey -L > "$tmpfile"
                BINDKEY_FILE="$tmpfile" my-generate-zsh-keybindings &>/dev/null &
                disown
                zle reset-prompt
              }
              zle -N my-export-zsh-keybindings
              bindkey '\e[1;4P' my-export-zsh-keybindings

              # colorize some commands, like ping, mount, uptime,  df
              source ${pkgs.grc}/etc/grc.zsh

              # autopair, e.g: echo | => " => echo "|"
              source ${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh
              # git tools, require fzf, (gbl - git blame; gcb = git checkout <branch>; grb - git rebase -i)
              source ${pkgs.zsh-forgit}/share/zsh/zsh-forgit/forgit.plugin.zsh
            '';
            # # Vi mode
            # # Improved vim bindings.
            # source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh

            shellAliases = { };

            plugins = [
              {
                name = "zsh-nix-shell";
                file = "nix-shell.plugin.zsh";
                src = pkgs.fetchFromGitHub {
                  owner = "chisui";
                  repo = "zsh-nix-shell";
                  rev = "v0.5.0";
                  sha256 =
                    "0za4aiwwrlawnia4f29msk822rj9bgcygw6a8a6iikiwzjjz0g91";
                };
              }
              {
                name = "term-title-support";
                file = "lib/termsupport.zsh";
                src = pkgs.fetchFromGitHub {
                  owner = "ohmyzsh";
                  repo = "ohmyzsh";
                  rev = "17ea97332b2f2285e3c2e1a00f6745fb1fe1cec5";
                  sha256 = "5/mRR/U7+WD0EE3cBZRl8YiyebDRsac4fOwCtrPrRDI=";
                };

              }
              {
                # ^g jump to bookmark, "mark" - add bookmark place
                name = "fzf-marks";
                file = "fzf-marks.plugin.zsh";
                src = pkgs.fetchFromGitHub {
                  owner = "urbainvaes";
                  repo = "fzf-marks";
                  rev = "2ff907f3e2e3959f038ee2a7d44d28c2b62ef38c";
                  sha256 = "/7zgbYylMGKerKhQS/C4PZ3Ghps3ooh9ASzGVu4itEs=";
                };
              }
            ];
          };
        };
      };
    };

    users.users.${cfg.name} = {
      isNormalUser = true;

      inherit (cfg) name;

      hashedPasswordFile = config.sops.secrets."users/userPassword".path;

      home = "/home/${cfg.name}";
      group = "users";

      shell = pkgs.zsh;

      # Arbitrary user ID to use for the user. Since I only
      # have a single user on my machines this won't ever collide.
      # However, if you add multiple users you'll need to change this
      # so each user has their own unique uid (or leave it out for the
      # system to select).
      uid = 1000;

      extraGroups = [ "wheel" ] ++ cfg.extraGroups;
    } // cfg.extraOptions;
  };
}
