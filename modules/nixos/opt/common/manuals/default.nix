{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.common.manuals;
  my-manual = pkgs.nerv.my-manual.overrideAttrs
    (old: { files = (old.files or [ ]) ++ cfg.files; });
in {
  options.nerv.opt.common.manuals = with types; {
    enable = mkBoolOpt false
      "Whether or not to create HTML manuals from markdown that are in this repo.";
    files = mkOpt (listOf path) [ ] "List of markdown files";
  };

  config = mkIf cfg.enable {
    nerv.opt.common.manuals.files = [ ../../../../../README.md ];
    nerv.home = {
      file."Manuals" = {
        recursive = true;
        source = "${my-manual}/manual";
      };
    };
  };
}
