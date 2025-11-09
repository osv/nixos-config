{ pkgs, ... }:

{
  "ebay".metaData.hidden = true;
  "amazondotcom-us".metaData.hidden = true;

  "AliExpress" = {
    urls = [{ template = "https://aliexpress.ru/wholesale?SearchText={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "ae01.alicdn.com/images/eng/wholesale/icon/aliexpress.ico";
                  sha256 = "sha256-7xgem2pY2PNuv8as1YnS+U03GvDLLGjhcDLt69rtmaA=";
              }}'';
    definedAliases = [ "@aliexpress" "@ali" ];
  };

  "Arch Wiki" = {
    urls = [{ template = "https://wiki.archlinux.org/index.php?search={searchTerms}"; }];
    icon = "${pkgs.fetchurl {
                  url = "https://wiki.archlinux.org/favicon.ico";
                  sha256 = "sha256-0uxMtT8myzTT7p9k6v5UxsguPKu+vHPlglNTMbnN1T0=";
              }}";
    definedAliases = [ "@archwiki" "@aw" ];
  };

  "crates.io" = {
    urls = [{ template = "https://crates.io/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://crates.io/favicon.ico";
                  sha256 = "sha256-upooA/+m5KMUD1t4WFY3EOmytdpUFgNqUj12Auta1mM=";
              }}'';
    definedAliases = [ "@crates" ];
  };

  "Discogs" = {
    urls = [{ template = "https://www.discogs.com/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://st.discogs.com/d56dcb7367720ea20f1b11a4385705517c7e7702/images/favicon.ico";
                  sha256 = "sha256-zEDrbmcUf8XHUyYzNc6JsWzBioX8sm8tjScGHim5VTk=";
              }}'';
    definedAliases = [ "@discogs" ];
  };

  "Docker Hub" = {
    urls = [{ template = "https://hub.docker.com/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://www.docker.com/wp-content/uploads/2023/04/cropped-Docker-favicon-32x32.png";
                  sha256 = "sha256-4NmHGMaq31qoIvdlmy7fI3qTbkcp1/tJhqQu/9Ci4/c=";
              }}'';
    definedAliases = [ "@dockerhub" "@docker" ];
  };

  "Ecosia Custom" = {
    urls = [{ template = "https://www.ecosia.org/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://cdn-static.ecosia.org/static/icons/favicon.ico";
                  sha256 = "sha256-uvPShG1yVh4C4zaJmGuhhr96V/NredB1Wte9O3U6QxA=";
              }}'';
    definedAliases = [ "@ecosia" ];
  };

  "Genius" = {
    urls = [{ template = "https://genius.com/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://assets.genius.com/images/apple-touch-icon.png";
                  sha256 = "sha256-M9YQEVg3T7hMO/xPfihR1aXfG+/pNiVOBCOtzx3GrkE=";
              }}'';
    definedAliases = [ "@genius" ];
  };

  "GitHub" = {
    urls = [{ template = "https://github.com/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://github.githubassets.com/favicons/favicon-dark.svg";
                  sha256 = "sha256-qu/d9ftvsntplFuxw9RFL8BpI9b2g5b6xfeGw6Ekh6w=";
              }}'';
    definedAliases = [ "@github" "@gh" ];
  };

  "godocs.io" = {
    urls = [{ template = "https://godocs.io/?q={}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://go.dev/images/favicon-gopher.svg";
                  sha256 = "sha256-OlKpUUeYF8TtMoX4e0ERK1ocIb53OJ8ZDxvwJaQVM/0=";
              }}'';
    definedAliases = [ "@godocs" ];
  };

  "pkgs.go.dev" = {
    urls = [{ template = "https://pkg.go.dev/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://go.dev/images/favicon-gopher.svg";
                  sha256 = "sha256-OlKpUUeYF8TtMoX4e0ERK1ocIb53OJ8ZDxvwJaQVM/0=";
              }}'';
    definedAliases = [ "@gopkgs" ];
  };

  "Hackage" = {
    urls = [{ template = "https://hackage.haskell.org/packages/search?terms={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://hackage.haskell.org/static/favicon.png";
                  sha256 = "sha256-+6WAv93yaA3L2eheGKxklY/uRAvbKD1q/WcmufmhKxY=";
              }}'';
    definedAliases = [ "@hackage" ];
  };

  "Hoogle" = {
    urls = [{ template = "https://hoogle.haskell.org/?hoogle={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://hoogle.haskell.org/favicon.png";
                  sha256 = "sha256-6qmjRYDDRUwm6EdLoZB6o9XtoujsfDEQJ9xOu3Knei8=";
              }}'';
    definedAliases = [ "@hoogle" ];
  };

  "Jisho" = {
    urls = [{ template = "https://jisho.org/search/{searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://assets.jisho.org/assets/favicon-062c4a0240e1e6d72c38aa524742c2d558ee6234497d91dd6b75a182ea823d65.ico";
                  sha256 = "sha256-BixKAkDh5tcsOKpSR0LC1VjuYjRJfZHda3WhguqCPWU=";
              }}'';
    definedAliases = [ "@jisho" ];
  };

  "コトバンク" = {
    urls = [{ template = "https://kotobank.jp/gs/?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://kotobank.jp/favicon.ico";
                  sha256 = "sha256-t+EzqURlQwznuBqa0GcBbqumvZqtU7HrEAjGUlqp1tg=";
              }}'';
    definedAliases = [ "@kotobank" ];
  };

  "Kubernetes" = {
    urls = [{ template = "https://kubernetes.io/search/?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://kubernetes.io/images/favicon.png";
                  sha256 = "sha256-YI5QvGQXoaTG3uUGQ/R99Xl2r+VqBAA1qqthzPbf8nQ=";
              }}'';
    definedAliases = [ "@kubernetes" "@k8s" ];
  };

  "Last.fm" = {
    urls = [{ template = "https://www.last.fm/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://www.last.fm/static/images/favicon.702b239b6194.ico";
                  sha256 = "sha256-ID+DfF+dZ5CzKiBp/psQPRD6r/06PZ0rVYiELWUt5Mw=";
              }}'';
    definedAliases = [ "@lastfm" ];
  };

  "MDN" = {
    urls = [{ template = "https://developer.mozilla.org/en-US/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://developer.mozilla.org/favicon-48x48.cbbd161b.png";
                  sha256 = "sha256-Wnd0BqQIKgroGmV+R8vqV9uNBwDvcxBrQ8hXOLOFeKY=";
              }}'';
    definedAliases = [ "@mdn" ];
  };

  "MELPA" = {
    urls = [{ template = "https://melpa.org/#/?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://melpa.org/favicon.ico";
                  sha256 = "sha256-bmlydqXBM8MUMC6cOTGSHPx6zN8tZFqmQ+srbXkSCA4=";
              }}'';
    definedAliases = [ "@melpa" ];
  };

  "MusicBrainz" = {
    urls = [{ template = "https://musicbrainz.org/search?type=artist&query={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://musicbrainz.org/static/images/favicons/favicon-16x16.png";
                  sha256 = "sha256-M5mKQurmO9AP0gfC+5OLwi8k4XWQy759eQrrKAeytl0=";
              }}'';
    definedAliases = [ "@musicbrainz" "@mb" ];
  };

  "NixOS Packages" = {
    urls = [{ template = "https://search.nixos.org/packages?channel=unstable&query={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://nixos.org/favicon.png";
                  sha256 = "sha256-awcsDbbpRcDJnJpRavj/IcKMReEektRcqKbE35IJTKQ=";
              }}'';
    definedAliases = [ "@nixpkgs" "@np" ];
  };

  "NixOS Options" = {
    urls = [{ template = "https://search.nixos.org/options?channel=unstable&query={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://nixos.org/favicon.png";
                  sha256 = "sha256-awcsDbbpRcDJnJpRavj/IcKMReEektRcqKbE35IJTKQ=";
              }}'';
    definedAliases = [ "@nixopts" "@no" ];
  };

  "NixOS Wiki" = {
    urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://nixos.wiki/favicon.png";
                  sha256 = "sha256-DE8IgVninF6Aq3iNMgerhvF1dpoXqDUSibtWSpf/dN4=";
              }}'';
    definedAliases = [ "@nixoswiki" "@nw" ];
  };

  "Nix Home Manager Options" = {
    urls = [{ template = "https://home-manager-options.extranix.com/?query={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://nixos.org/favicon.png";
                  sha256 = "sha256-awcsDbbpRcDJnJpRavj/IcKMReEektRcqKbE35IJTKQ=";
              }}'';
    definedAliases = [ "@homemanager" "@hm" ];
  };

  "OpenStreetMap" = {
    urls = [{ template = "https://www.openstreetmap.org/search?query={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://www.openstreetmap.org/assets/favicon-32x32-99b88fcadeef736889823c8a886b89d8cada9d4423a49a27de29bacc0a6bebd1.png";
                  sha256 = "sha256-dt4QVbQPdb4neS/fwH3yOWOSbEdkjMZtAYnIeCfr7qI=";
              }}'';
    definedAliases = [ "@openstreetmap" "@osm" "@maps" ];
  };

  "ProtonDB" = {
    urls = [{ template = "https://www.protondb.com/search?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://www.protondb.com/sites/protondb/images/favicon.ico";
                  sha256 = "sha256-oauOp0EASNjMcThfzYJ2TfbaOYHBPL8LOp+9lmp4pmc=";
              }}'';
    definedAliases = [ "@protondb" ];
  };

  "PyPI" = {
    urls = [{ template = "https://pypi.org/search/?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://pypi.org/static/images/logo-small.2a411bc6.svg";
                  sha256 = "sha256-+fcSfcNxAMLIFkp+gh52c48lQORoyhcegUIFtuq/zYs=";
              }}'';
    definedAliases = [ "@pypi" ];
  };

  "Python Docs" = {
    urls = [{ template = "https://docs.python.org/3/search.html?q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://docs.python.org/3/_static/py.svg";
                  sha256 = "sha256-WGW+i8wK+IhZSQPqARL2yNkjxXJsQIHoyFYRDMcznO8=";
              }}'';
    definedAliases = [ "@pydocs" ];
  };

  "Rate Your Music" = {
    urls = [{ template = "https://rateyourmusic.com/search?searchterm={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://e.snmc.io/3.0/img/logo/sonemic-32.png";
                  sha256 = "sha256-JpTt1tjBkUvDMTGrG7Hg2EiE8PR3RL7McodeZk1EpZA=";
              }}'';
    definedAliases = [ "@rym" ];
  };

  "Rust Std" = {
    urls = [{ template = "https://doc.rust-lang.org/std/?search={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://www.rust-lang.org/static/images/favicon-32x32.png";
                  sha256 = "sha256-l2y4jpnODbua4dyLvXTMBlHVkoDPM9y00l6L61so7eA=";
              }}'';
    definedAliases = [ "@ruststd" "@rust" ];
  };

  "SourceHut" = {
    urls = [{ template = "https://sr.ht/projects?search={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://sr.ht/static/logo.png";
                  sha256 = "sha256-NBzKZhqE9//zVJlOwYiwyW/jRFh8+nS2YvC3zMCQ1fU=";
              }}'';
    definedAliases = [ "@sourcehut" "@srht" ];
  };

  "SteamDB" = {
    urls = [{ template = "https://steamdb.info/search/?a=app&q={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://steamdb.info/static/logos/32px.png";
                  sha256 = "sha256-IUBiB5JUSvyDa+m/wecmHB8s3Wfu0JK98bJ+ZRZ5ybQ=";
              }}'';
    definedAliases = [ "@steamdb" ];
  };

  "WolframAlpha" = {
    urls = [{ template = "https://www.wolframalpha.com/input?i={searchTerms}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://www.wolframalpha.com/_next/static/images/favicon_1zbE9hjk.ico";
                  sha256 = "sha256-S9k7AlBQiDElBCGopJ8xfBD6dIhGU+EBh8t1QYbP2S4=";
              }}'';
    definedAliases = [ "@wolframalpha" "@wa" ];
  };

  "YouTube Custom" = {
    urls = [{ template = "https://yewtu.be/search?q={}"; }];
    icon = ''${pkgs.fetchurl {
                  url = "https://www.youtube.com/s/desktop/280a3f09/img/favicon.ico";
                  sha256 = "sha256-i7HQ+kOhdDbVndVG9vdMdtxEc13vdSLCLYAxFm24kR0=";
              }}'';
    definedAliases = [ "@youtube" "@yt" ];
  };
}
