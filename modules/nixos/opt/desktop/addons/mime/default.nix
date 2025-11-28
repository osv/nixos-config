{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.mime;
  browser = [ "firefox.desktop" ];
  player = [ "vlc.desktop" ];
  editor = [ "emacsclient.desktop" ];
  imageViewer = [ "qimgv.desktop" ];
  pdfViewer = [ "org.pwmt.zathura.desktop" ];

  associations = {
    "inode/directory" = [ "org.kde.dolphin.desktop" ];

    # Browser associations
    "text/html" = browser;
    "x-scheme-handler/http" = browser;
    "x-scheme-handler/https" = browser;
    "x-scheme-handler/ftp" = browser;
    "x-scheme-handler/chrome" = browser;
    "x-scheme-handler/about" = browser;
    "x-scheme-handler/unknown" = browser;
    "application/x-extension-htm" = browser;
    "application/x-extension-html" = browser;
    "application/x-extension-shtml" = browser;
    "application/xhtml+xml" = browser;
    "application/x-extension-xhtml" = browser;
    "application/x-extension-xht" = browser;

    # Video associations
    "video/x-flic" = player;
    "video/mpeg" = player;
    "video/x-ms-wmv" = player;
    "video/vnd.rn-realvideo" = player;
    "video/x-theora+ogg" = player;
    "video/dv" = player;
    "video/webm" = player;
    "video/ogg" = player;
    "video/quicktime" = player;
    "video/x-flv" = player;
    "video/x-ogm+ogg" = player;
    "video/3gpp2" = player;
    "video/mp2t" = player;
    "video/x-msvideo" = player;
    "video/3gpp" = player;
    "video/x-matroska" = player;
    "video/vnd.mpegurl" = player;
    "video/mp4" = player;

    # Audio associations
    "audio/aac" = player;
    "audio/ac3" = player;
    "audio/x-wavpack" = player;
    "audio/webm" = player;
    "audio/x-ms-wma" = player;
    "audio/flac" = player;
    "audio/x-scpls" = player;
    "audio/mpeg" = player;
    "audio/x-mpegurl" = player;
    "audio/x-ms-asx" = player;
    "audio/vnd.rn-realaudio" = player;
    "audio/x-wav" = player;
    "audio/vnd.dts" = player;
    "audio/x-adpcm" = player;
    "audio/x-vorbis+ogg" = player;
    "audio/mp4" = player;
    "audio/x-tta" = player;
    "audio/x-musepack" = player;
    "audio/AMR" = player;
    "audio/x-matroska" = player;
    "audio/x-ape" = player;
    "audio/x-aiff" = player;
    "audio/vnd.dts.hd" = player;
    "audio/ogg" = player;
    "audio/mp2" = player;

    # Text/Document associations
    "text/plain" = editor;
    "text/x-makefile" = editor;
    "text/x-c++hdr" = editor;
    "text/x-c++src" = editor;
    "text/x-chdr" = editor;
    "text/x-csrc" = editor;
    "text/x-java" = editor;
    "text/x-moc" = editor;
    "text/x-pascal" = editor;
    "text/tcl" = editor;
    "text/x-tex" = editor;
    "text/x-python" = editor;
    "text/vbscript" = editor;
    "text/spreadsheet" = editor;
    "text/tab-separated-values" = editor;
    "text/csv" = editor;

    # Image associations
    "image/jpeg" = imageViewer;
    "image/jpg" = imageViewer;
    "image/png" = imageViewer;
    "image/gif" = imageViewer;
    "image/bmp" = imageViewer;
    "image/tiff" = imageViewer;
    "image/webp" = imageViewer;
    "image/svg+xml" = imageViewer;
    "image/x-portable-pixmap" = imageViewer;
    "image/x-portable-greymap" = imageViewer;
    "image/x-portable-bitmap" = imageViewer;
    "image/x-portable-anymap" = imageViewer;

    # PDF and document associations
    "application/pdf" = pdfViewer;
    "application/x-pdf" = pdfViewer;
    "application/postscript" = pdfViewer;
    "application/x-bzpdf" = pdfViewer;
    "application/x-gzpdf" = pdfViewer;
    "application/x-xzpdf" = pdfViewer;
    "application/oxps" = pdfViewer;
    "application/epub+zip" = pdfViewer;
    "application/x-fictionbook+xml" = pdfViewer;
    "image/vnd.djvu" = pdfViewer;
    "image/x-djvu" = pdfViewer;
    "application/x-cbr" = pdfViewer;
    "application/x-cbz" = pdfViewer;
    "application/x-cb7" = pdfViewer;
    "application/x-cbt" = pdfViewer;
  };
in
{
  options.nerv.opt.desktop.addons.mime = with types; {
    enable = mkBoolOpt false "Whether to customize MIME apps";
  };

  config = mkIf cfg.enable {
    nerv.home.extraOptions = {
      # xdg.configFile."mimeapps.list".force = true; # too many apps replace this link when opened to add their own definitions
      xdg.mime.enable = true;
      xdg.mimeApps.enable = true;
      xdg.mimeApps.associations.added = associations;
      xdg.mimeApps.defaultApplications = associations;
    };
  };
}
