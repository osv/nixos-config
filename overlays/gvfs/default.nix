{ channels, ... }:

final: prev: {
  gvfs = prev.gvfs.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      # Change name of volumes
      # https://www.linux.org.ru/gallery/screenshots/17407707
      # https://github.com/sde-gui/PKGBUILDs-gnome-platform-patches/blob/main/gvfs/0001-gvfs-improve-volume-names-and-sorting.diff
      ./0001-gvfs-improve-volume-names-and-sorting.diff
    ];
  });
}
