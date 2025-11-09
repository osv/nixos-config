{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.tools.bottom"
    "system monitor for displaying resource usage"
    (with pkgs; [ bottom ])
    {}
    {}
  ]

  [
    "nerv.opt.tools.fup-repl"
    "interactive REPL for fup language"
    (with pkgs; [
      (writeShellScriptBin "fup-repl" ''
        exec ${fup-repl}/bin/fup-repl "$$@"
      '')
    ])
    {}
    {}
  ]

  [
    "nerv.opt.tools.http"
    "common HTTP utilities"
    (with pkgs; [ wget curl ])
    {}
    {}
  ]

  [
    "nerv.opt.tools.appimage-run"
    "tool for running AppImage executables"
    (with pkgs; [ appimage-run ])
    {}
    {
      nerv.home.configFile."wgetrc".text = "";
    }
  ]

  [
    "nerv.opt.tools.go"
    "Go programming language support with language server"
    (with pkgs; [ go gopls ])
    {}
    {
      nerv.home.extraOptions.home.sessionVariables.GOPATH = "$HOME/work/go";
    }
  ]

  [
    "nerv.opt.tools.steganography"
    "steganography tools for hiding files in images with double encryption"
    (with pkgs; [ nerv.steganography-tools ])
    {}
    {}
  ]
]
