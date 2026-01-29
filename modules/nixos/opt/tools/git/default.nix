{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.tools.git;
  gpg = config.nerv.opt.security.gpg;
  user = config.nerv.opt.user;
in
{
  options.nerv.opt.tools.git = with types; {
    enable = mkBoolOpt false "Whether or not to install and configure git.";
    userName = mkOpt types.str user.fullName "The name to configure git with.";
    userEmail = mkOpt types.str user.email "The email to configure git with.";
    signingKey =
      mkOpt types.str "B33D6CC4C3A13CF7" "The key ID to sign commits with.";
    ghq = mkBoolOpt true "Whether or not to enable repository management by ghq.";
    github = mkBoolOpt true "Whether or not to enable github tool.";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.systemPackages = with pkgs; [ gitFull delta ];

      nerv.home.extraOptions = {
        programs = {
          git = {
            enable = true;
            lfs = on;
            signing = {
              key = cfg.signingKey;
              signByDefault = mkIf gpg.enable true;
            };
            settings = {
              user = {
                name = cfg.userName;
                email = cfg.userEmail;
              };
              init = { defaultBranch = "master"; };
              pull = { rebase = true; };
              push = { autoSetupRemote = true; };
              core = {
                whitespace = "trailing-space,space-before-tab";
                pager = "delta";
              };
              interactive = { diffFilter = "delta --color-only"; };
              include.path = "${toString ./delta-theme.gitconfig}";
              delta = {
                navigate = true; # use n and N to move between diff sections
                # set to true if you're in a terminal w/ a light background color
                light = false; # todo - respect theme?
                side-by-side = true;
                features = "chameleon"; # theme
              };
              safe = {
                directory = "${config.users.users.${user.name}.home}/work/my/nixos-config";
              };
            };
          };
          gh = {
            enable = cfg.github;
            # TODO: Add extension: https://github.com/kawarimidoll/gh-q
            settings.aliases = { co = "pr checkout"; pv = "pr view"; };
          };

        };
      };
    })
    (mkIf (cfg.enable && cfg.ghq) {
      environment.systemPackages = with pkgs; [ ghq ];

      nerv.home.extraOptions = {
        programs.zsh = {
          initContent = ''
source ${pkgs.ghq}/share/zsh/site-functions/_ghq

# ghq project switcher with fzf
function ghq_cd() {
  local proj=$(${lib.getExe pkgs.ghq} list -p | ${lib.getExe pkgs.fzf} \
      --no-multi \
      --prompt "Project: " \
      --preview "${lib.getExe pkgs.lsd} -1FAL --group-dirs first --icon always --color always {}")
  if [[ -n "$proj" ]]; then
    cd -- "$proj"
  fi
}

'';
          shellAliases = { gg = "${lib.getExe pkgs.ghq} get"; };
        };
        programs.git.settings = {
          ghq = {
            root = [
              "~/work/other/nix-repos" # example of nix repos for inspiration
              "~/work/other/repos" # primary
            ];
            "https://github.com/osv" = { root = "~/work/my/repos"; };
            "ssh://git@github.com/osv" = { root = "~/work/my/repos"; };
            "ssh://git@github.com:osv" = { root = "~/work/my/repos"; };
            "https://github.com/Kayzen" = { root = "~/work/kayzen/repos"; };
            "ssh://git@github.com/Kayzen" = { root = "~/work/kayzen/repos"; };
            "ssh://git@github.com:Kayzen" = { root = "~/work/kayzen/repos"; };
          };
        };
      };
    })
  ];
}
