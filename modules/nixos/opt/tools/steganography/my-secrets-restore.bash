#!/usr/bin/env bash
set -euo pipefail

umask 077

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

die() {
  printf '%bError: %s%b\n' "$RED" "$*" "$NC" >&2
  exit 1
}

info() {
  printf '%b%s%b\n' "$YELLOW" "$*" "$NC"
}

success() {
  printf '%b%s%b\n' "$GREEN" "$*" "$NC"
}

read_secret() {
  local prompt=$1
  REPLY=
  printf '%b%s: %b' "$YELLOW" "$prompt" "$NC" >&2
  IFS= read -r -s REPLY
  printf '\n' >&2
  [[ -n $REPLY ]] || die "Password cannot be empty"
}

[[ $# -eq 2 ]] || die "Usage: my-secrets-restore <image> <output-directory>"

image=$1
output_dir=$2

[[ -f $image ]] || die "Image not found: $image"

if [[ -e $output_dir && ! -d $output_dir ]]; then
  die "Output path is not a directory: $output_dir"
fi

mkdir -p -- "$output_dir"

if find "$output_dir" -mindepth 1 -maxdepth 1 -print -quit | grep -q .; then
  printf '%bOutput directory is not empty. Existing files may be overwritten. Continue? [y/N] %b' "$YELLOW" "$NC"
  IFS= read -r answer
  case $answer in
    y|Y|yes|YES) ;;
    *) die "Cancelled" ;;
  esac
fi

tmp_dir=$(mktemp -d)
trap 'rm -rf -- "$tmp_dir"' EXIT
archive=$tmp_dir/hidden.7z
staging=$tmp_dir/extracted
mkdir -p -- "$staging"

read_secret "Steghide password"
steg_password=$REPLY
read_secret "Archive password"
archive_password=$REPLY

info '[1/3] Extracting encrypted archive from image'
steghide extract \
  -sf "$image" \
  -xf "$archive" \
  -p "$steg_password" \
  -f >/dev/null 2>&1 \
  || die "Steghide extraction failed (wrong password or invalid image)"
unset steg_password

info '[2/3] Decrypting archive'
7z x \
  -p"$archive_password" \
  -o"$staging" \
  -y \
  "$archive" >/dev/null \
  || die "Archive extraction failed (wrong password or damaged archive)"
unset archive_password REPLY

info '[3/3] Copying files to destination'
cp -a -- "$staging"/. "$output_dir"/

info 'Restored files:'
find "$staging" -type f -printf '  %P\n'
success "Done: $output_dir"
