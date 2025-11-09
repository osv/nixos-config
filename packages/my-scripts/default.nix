{ pkgs, lib, ... }:

pkgs.writeShellScriptBin "my-create-git-clone-script" ''
  echo "#!/usr/bin/env sh"
  find . -maxdepth 3 -name config -type f -exec grep -A 1 -e 'remote "origin"' \{\} + \
     | grep url \
     | sed 's/.*url = /git clone --depth 1 /g' \
     | sed 's_\(\([a-zA-Z0-9.-]\+\)/\([a-zA-Z0-0]\+\)[a-zA-Z0-9.-]\+\)$_\1 \2--\3 _'
''
