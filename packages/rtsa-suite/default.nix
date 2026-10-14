{
  lib,
  stdenv,
  pkgs,
  inputs,
  ...
}:

let
  version = "3.0.3.16655";

  # Aaronia keeps downloads.aaronia.com behind a Cloudflare challenge, so
  # fetchurl does not work. The tarball is wired as the rtsa-suite-tarball
  # path input (see flake.nix): download it manually from
  # https://aaronia.com/en/support/downloads and place it into
  # /persist/derivative/home/osv/dists/rtsa-suite-<version>/.
  # To upgrade: replace the tarball, update the input url and expectedHash.
  tarball = "${inputs.rtsa-suite-tarball}/aaronia-rtsa-suite-${version}-Linux.tar.gz";
  expectedHash = "04d58c409ab80ddafc3b2f42f916985555eb8c5cb861130f17fecb1de7d6d8d0";

  runtimeLibs = with pkgs; [
    gcc.cc.lib # libstdc++
    glib
    openssl
    dbus.lib
    krb5.lib
    fontconfig.lib
    freetype
    libpulseaudio
    libusb1
    libxkbcommon
    libglvnd
    libxcb
    libxcb-cursor
    xcbutilwm
    xcbutilkeysyms
    xcbutilimage
    xcbutilrenderutil
    xorg.libX11
    xorg.libXrandr
    libdrm
    mesa # libgbm (Qt eglfs plugins)
    systemd # libudev
    zlib
    zstd
  ];

  # Unused optional Qt plugins: printing, text-to-speech and SQL drivers.
  # Add cups, speechd, postgresql etc. to runtimeLibs if ever needed.
  ignoredMissingDeps = [
    "libcups.so.2"
    "libspeechd.so.2"
    "libmimerapi.so"
    "libpq.so.5"
    "libmysqlclient.so.21"
    "libodbc.so.2"
  ];
in
stdenv.mkDerivation {
  pname = "aaronia-rtsa-suite";
  inherit version;

  src =
    if builtins.hashFile "sha256" tarball == expectedHash then
      tarball
    else
      throw ''
        aaronia-rtsa-suite-${version}: tarball is missing or has a wrong hash.
        Download it from https://aaronia.com/en/support/downloads (Linux X86 64-bit tar.gz)
        and place it into /persist/derivative/home/osv/dists/.'';

  sourceRoot = ".";

  nativeBuildInputs = with pkgs; [
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = runtimeLibs;

  # Qt loads some libraries via dlopen (libxcb-cursor and the xcb-util
  # family), so all lib dirs are added to RUNPATH.
  appendRunpaths = map (p: "${lib.getLib p}/lib") runtimeLibs;

  autoPatchelfIgnoreMissingDeps = ignoredMissingDeps;

  installPhase = ''
    runHook preInstall

    appDir=$out/opt/aaronia-rtsa-suite
    mkdir -p $appDir $out/bin $out/share/applications $out/share/icons/hicolor/256x256/apps

    cp -r aaronia-rtsa-suite-${version}-Linux/opt/aaronia-rtsa-suite/. $appDir/
    install -Dm644 $appDir/Aaronia-RTSA-Suite-PRO/RTSA-256.png \
      $out/share/icons/hicolor/256x256/apps/aaronia-rtsa-suite.png

    makeWrapper $appDir/Aaronia-RTSA-Suite-PRO/Aaronia-RTSA-Suite-PRO \
      $out/bin/aaronia-rtsa-suite \
      --run "cd $appDir/Aaronia-RTSA-Suite-PRO"

    makeWrapper $appDir/Aaronia-RTSA-Suite-PRO/RTSAFileTool \
      $out/bin/aaronia-rtsa-filetool \
      --run "cd $appDir/Aaronia-RTSA-Suite-PRO"

    # The vendor desktop file points to /opt — write our own.
    cat > $out/share/applications/aaronia-rtsa-suite.desktop << 'EOF'
    [Desktop Entry]
    Type=Application
    Name=RTSA-Suite PRO
    Comment=Aaronia AG RTSA-Suite PRO Desktop
    Icon=aaronia-rtsa-suite
    Exec=aaronia-rtsa-suite
    Terminal=false
    Categories=Science;Engineering;
    EOF

    runHook postInstall
  '';

  meta = {
    homepage = "https://aaronia.com/en/products/software/rtsa-software";
    license = lib.licenses.unfree;
    description = "Aaronia RTSA-Suite PRO — Spectrum Analyzer Software for SPECTRAN V6";
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
