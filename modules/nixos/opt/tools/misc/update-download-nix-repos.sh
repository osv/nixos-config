#!/usr/bin/env sh
#
echo "Updating download-nix-repos.sh"
(cd ~/work/other/nix-repos && my-create-git-clone-script > ./download-nix-repos.sh)
