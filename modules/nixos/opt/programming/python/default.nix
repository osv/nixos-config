{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.programming.python;
in {
  options.nerv.opt.programming.python = with types; {
    enable = mkBoolOpt false "Whether or not to install and configure Python";
    pkg = mkOpt package pkgs.python3 "The Python package to use";
    uv = {
      enable = mkBoolOpt true "Whether or not to install uv";
      pkg = mkOpt package pkgs.uv "The uv package to use";
    };
    ruff = {
      enable = mkBoolOpt true "Whether or not to install ruff";
      pkg = mkOpt package pkgs.ruff "The ruff package to use";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ cfg.pkg python3Packages.pyarrow ]
      ++ (lib.optional cfg.uv.enable cfg.uv.pkg)
      ++ (lib.optional cfg.ruff.enable cfg.ruff.pkg);
  };
}
