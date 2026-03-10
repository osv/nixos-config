{
  lib,
  pkgs,
  ...
}:

pkgs.buildNimPackage {
  pname = "boomer";
  version = "unstable-2024-02-08";

  src = pkgs.fetchFromGitHub {
    owner = "tsoding";
    repo = "boomer";
    rev = "dfd4e1f5514e2a9d7c7a6429c1c0642c2021e792";
    hash = "sha256-o65/VVxttriA5Qqt35lLKkWIZYS7T4VBBuYdAIGUmx8=";
  };

  lockFile = ./lock.json;

  patches = [ ./aa-flashpoint.patch ];

  buildInputs = with pkgs; [
    xorg.libX11
    xorg.libXrandr
    xorg.libXext
    libGL
    libGLU
  ];

  meta = with lib; {
    description = "Zoom and enhance desktop";
    homepage = "https://github.com/tsoding/boomer";
    license = licenses.mit;
    platforms = platforms.linux;
    mainProgram = "boomer";
  };
}
