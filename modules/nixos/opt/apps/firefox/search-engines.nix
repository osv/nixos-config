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
    icon = "${./favicons/archlinux.ico}";
    definedAliases = [ "@archwiki" "@aw" ];
  };

  "crates.io" = {
    urls = [{ template = "https://crates.io/search?q={searchTerms}"; }];
    icon = "${./favicons/crates.ico}";
    definedAliases = [ "@crates" ];
  };

  "Discogs" = {
    urls = [{ template = "https://www.discogs.com/search?q={searchTerms}"; }];
    icon = "${./favicons/discogs.ico}";
    definedAliases = [ "@discogs" ];
  };

  "Docker Hub" = {
    urls = [{ template = "https://hub.docker.com/search?q={searchTerms}"; }];
    icon = "${./favicons/docker.png}";
    definedAliases = [ "@dockerhub" "@docker" ];
  };

  "Ecosia Custom" = {
    urls = [{ template = "https://www.ecosia.org/search?q={searchTerms}"; }];
    icon = "${./favicons/ecosia.ico}";
    definedAliases = [ "@ecosia" ];
  };

  "Genius" = {
    urls = [{ template = "https://genius.com/search?q={searchTerms}"; }];
    icon = "${./favicons/genius.png}";
    definedAliases = [ "@genius" ];
  };

  "GitHub" = {
    urls = [{ template = "https://github.com/search?q={searchTerms}"; }];
    icon = "${./favicons/github.svg}";
    definedAliases = [ "@github" "@gh" ];
  };

  "godocs.io" = {
    urls = [{ template = "https://godocs.io/?q={}"; }];
    icon = "${./favicons/go.svg}";
    definedAliases = [ "@godocs" ];
  };

  "pkgs.go.dev" = {
    urls = [{ template = "https://pkg.go.dev/search?q={searchTerms}"; }];
    icon = "${./favicons/go.svg}";
    definedAliases = [ "@gopkgs" ];
  };

  "Hackage" = {
    urls = [{ template = "https://hackage.haskell.org/packages/search?terms={searchTerms}"; }];
    icon = "${./favicons/hackage.png}";
    definedAliases = [ "@hackage" ];
  };

  "Hoogle" = {
    urls = [{ template = "https://hoogle.haskell.org/?hoogle={searchTerms}"; }];
    icon = "${./favicons/hoogle.png}";
    definedAliases = [ "@hoogle" ];
  };

  "Jisho" = {
    urls = [{ template = "https://jisho.org/search/{searchTerms}"; }];
    icon = "${./favicons/jisho.ico}";
    definedAliases = [ "@jisho" ];
  };

  "コトバンク" = {
    urls = [{ template = "https://kotobank.jp/gs/?q={searchTerms}"; }];
    icon = "${./favicons/kotobank.ico}";
    definedAliases = [ "@kotobank" ];
  };

  "Kubernetes" = {
    urls = [{ template = "https://kubernetes.io/search/?q={searchTerms}"; }];
    icon = "${./favicons/kubernetes.png}";
    definedAliases = [ "@kubernetes" "@k8s" ];
  };

  "Last.fm" = {
    urls = [{ template = "https://www.last.fm/search?q={searchTerms}"; }];
    icon = "${./favicons/lastfm.ico}";
    definedAliases = [ "@lastfm" ];
  };

  "MDN" = {
    urls = [{ template = "https://developer.mozilla.org/en-US/search?q={searchTerms}"; }];
    icon = "${./favicons/mdn.png}";
    definedAliases = [ "@mdn" ];
  };

  "MELPA" = {
    urls = [{ template = "https://melpa.org/#/?q={searchTerms}"; }];
    icon = "${./favicons/melpa.ico}";
    definedAliases = [ "@melpa" ];
  };

  "MusicBrainz" = {
    urls = [{ template = "https://musicbrainz.org/search?type=artist&query={searchTerms}"; }];
    icon = "${./favicons/musicbrainz.png}";
    definedAliases = [ "@musicbrainz" "@mb" ];
  };

  "NixOS Packages" = {
    urls = [{ template = "https://search.nixos.org/packages?channel=unstable&query={searchTerms}"; }];
    icon = "${./favicons/nixos.png}";
    definedAliases = [ "@nixpkgs" "@np" ];
  };

  "NixOS Options" = {
    urls = [{ template = "https://search.nixos.org/options?channel=unstable&query={searchTerms}"; }];
    icon = "${./favicons/nixos.png}";
    definedAliases = [ "@nixopts" "@no" ];
  };

  "NixOS Wiki" = {
    urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}"; }];
    icon = "${./favicons/nixos-wiki.png}";
    definedAliases = [ "@nixoswiki" "@nw" ];
  };

  "Nix Home Manager Options" = {
    urls = [{ template = "https://home-manager-options.extranix.com/?query={searchTerms}"; }];
    icon = "${./favicons/nixos.png}";
    definedAliases = [ "@homemanager" "@hm" ];
  };

  "OpenStreetMap" = {
    urls = [{ template = "https://www.openstreetmap.org/search?query={searchTerms}"; }];
    icon = "${./favicons/openstreetmap.png}";
    definedAliases = [ "@openstreetmap" "@osm" "@maps" ];
  };

  "ProtonDB" = {
    urls = [{ template = "https://www.protondb.com/search?q={searchTerms}"; }];
    icon = "${./favicons/protondb.ico}";
    definedAliases = [ "@protondb" ];
  };

  "PyPI" = {
    urls = [{ template = "https://pypi.org/search/?q={searchTerms}"; }];
    icon = "${./favicons/pypi.svg}";
    definedAliases = [ "@pypi" ];
  };

  "Python Docs" = {
    urls = [{ template = "https://docs.python.org/3/search.html?q={searchTerms}"; }];
    icon = "${./favicons/python.svg}";
    definedAliases = [ "@pydocs" ];
  };

  "Rate Your Music" = {
    urls = [{ template = "https://rateyourmusic.com/search?searchterm={searchTerms}"; }];
    icon = "${./favicons/rateyourmusic.png}";
    definedAliases = [ "@rym" ];
  };

  "Rust Std" = {
    urls = [{ template = "https://doc.rust-lang.org/std/?search={searchTerms}"; }];
    icon = "${./favicons/rust.png}";
    definedAliases = [ "@ruststd" "@rust" ];
  };

  "SourceHut" = {
    urls = [{ template = "https://sr.ht/projects?search={searchTerms}"; }];
    icon = "${./favicons/sourcehut.png}";
    definedAliases = [ "@sourcehut" "@srht" ];
  };

  "SteamDB" = {
    urls = [{ template = "https://steamdb.info/search/?a=app&q={searchTerms}"; }];
    icon = "${./favicons/steamdb.png}";
    definedAliases = [ "@steamdb" ];
  };

  "WolframAlpha" = {
    urls = [{ template = "https://www.wolframalpha.com/input?i={searchTerms}"; }];
    icon = "${./favicons/wolfram.ico}";
    definedAliases = [ "@wolframalpha" "@wa" ];
  };

  "YouTube Custom" = {
    urls = [{ template = "https://yewtu.be/search?q={}"; }];
    icon = "${./favicons/youtube.ico}";
    definedAliases = [ "@youtube" "@yt" ];
  };
}
