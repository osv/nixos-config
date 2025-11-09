#!/usr/bin/env bash
set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

error() {
  echo -e "${RED}Error: $1${NC}" >&2
  exit 1
}

success() {
  echo -e "${GREEN}$1${NC}"
}

info() {
  echo -e "${YELLOW}$1${NC}"
}

# Securely read password
read_password() {
  local prompt="$1"
  local password=""
  local password_confirm=""

  while true; do
    echo -n "$prompt: " >&2
    read -rs password
    echo >&2

    if [ -z "$password" ]; then
      error "Password cannot be empty"
    fi

    echo -n "Confirm password: " >&2
    read -rs password_confirm
    echo >&2

    if [ "$password" = "$password_confirm" ]; then
      echo "$password"
      return 0
    else
      echo -e "${RED}Passwords do not match. Please try again.${NC}" >&2
    fi
  done
}

# Hide command
cmd_hide() {
  if [ $# -lt 3 ]; then
    error "Usage: steg hide <cover-image> <output-image> <file1> [file2] [dir1] ..."
  fi

  local cover_image="$1"
  local output_image="$2"
  shift 2
  local files_to_hide=("$@")

  # Validate cover image exists
  if [ ! -f "$cover_image" ]; then
    error "Cover image not found: $cover_image"
  fi

  # Validate all input files/directories exist
  for item in "${files_to_hide[@]}"; do
    if [ ! -e "$item" ]; then
      error "File or directory not found: $item"
    fi
  done

  # Create temporary directory for work
  local tmp_dir
  tmp_dir=$(mktemp -d)
  trap 'rm -rf "$tmp_dir"' EXIT

  local archive="$tmp_dir/hidden.7z"

  info "Step 1/3: Creating encrypted 7z archive..."
  local password_7z
  password_7z=$(read_password "Enter password for 7z encryption")

  # Create 7z archive with password
  7z a -p"$password_7z" -mhe=on "$archive" "${files_to_hide[@]}" > /dev/null || error "Failed to create 7z archive"
  success "Archive created successfully"

  info "Step 2/3: Embedding archive into image..."
  local password_steg
  password_steg=$(read_password "Enter password for steganography")

  # Embed archive into image
  steghide embed -cf "$cover_image" -ef "$archive" -sf "$output_image" -p "$password_steg" -f > /dev/null 2>&1 || error "Failed to embed data into image"
  success "Data embedded successfully"

  info "Step 3/3: Finalizing..."
  success "Complete! Steganographic image saved to: $output_image"

  # Show file sizes
  local cover_size output_size
  cover_size=$(du -h "$cover_image" | cut -f1)
  output_size=$(du -h "$output_image" | cut -f1)
  echo "Cover image size: $cover_size"
  echo "Output image size: $output_size"
}

# Extract command
cmd_extract() {
  if [ $# -lt 1 ]; then
    error "Usage: steg extract <stego-image> [output-directory]"
  fi

  local stego_image="$1"
  local output_dir="${2:-.}"

  # Validate stego image exists
  if [ ! -f "$stego_image" ]; then
    error "Stego image not found: $stego_image"
  fi

  # Create output directory if it doesn't exist
  mkdir -p "$output_dir"

  # Create temporary directory for work
  local tmp_dir
  tmp_dir=$(mktemp -d)
  trap 'rm -rf "$tmp_dir"' EXIT

  local archive="$tmp_dir/extracted.7z"

  info "Step 1/3: Extracting hidden archive from image..."
  echo -n "Enter steganography password: " >&2
  read -rs password_steg
  echo >&2

  # Extract archive from image
  steghide extract -sf "$stego_image" -xf "$archive" -p "$password_steg" -f > /dev/null 2>&1 || error "Failed to extract data from image (wrong password or no hidden data)"
  success "Archive extracted from image"

  info "Step 2/3: Decrypting 7z archive..."
  echo -n "Enter 7z password: " >&2
  read -rs password_7z
  echo >&2

  # Extract files from 7z archive
  7z x -p"$password_7z" -o"$output_dir" "$archive" > /dev/null || error "Failed to extract 7z archive (wrong password?)"
  success "Archive decrypted successfully"

  info "Step 3/3: Finalizing..."
  success "Complete! Files extracted to: $output_dir"

  # List extracted files
  echo "Extracted files:"
  ls -lh "$output_dir"
}

# Main command dispatcher
case "${1:-}" in
  hide)
    shift
    cmd_hide "$@"
    ;;
  extract)
    shift
    cmd_extract "$@"
    ;;
  *)
    echo "Usage: steg <command> [options]"
    echo ""
    echo "Commands:"
    echo "  hide <cover-image> <output-image> <file1> [file2] [dir1] ..."
    echo "      Hide files/directories in an image using double encryption"
    echo ""
    echo "  extract <stego-image> [output-directory]"
    echo "      Extract hidden files from a steganographic image"
    echo ""
    echo "Example:"
    echo "  steg hide cover.jpg secret.jpg passwords.txt documents/"
    echo "  steg extract secret.jpg extracted/"
    exit 1
    ;;
esac
