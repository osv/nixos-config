#!/usr/bin/env bash
# Original script: https://raw.githubusercontent.com/ryanoasis/nerd-fonts/master/bin/scripts/test-fonts.sh

# Given an array of decimal numbers print all unicode codepoint.
function print-decimal-unicode-range() {
  local originalSequence=("$@")
  local counter=0
  local currentColorCode="\033[38;5;2m"
  local currentColorChar="\033[38;5;15m"
  local reset_color='\033[0m'
  local allChars=""
  local allCodes=""
  local wrapAt=16
  [[ "$wrappingValue" =~ ^[0-9]+$ ]] && [ "$wrappingValue" -gt 2 ] && wrapAt="$wrappingValue"
  local originalSequenceLength=${#originalSequence[@]}
  local leftoverSpaces=$((wrapAt - (originalSequenceLength % wrapAt)))

  # add fillers to array to maintain table:
  if [ "$leftoverSpaces" -lt "$wrapAt" ]; then
    for ((c = 1; c <= leftoverSpaces; c++)); do
      originalSequence+=(0)
    done
  fi

  for decimalCode in "${originalSequence[@]}"; do
    local hexCode
    hexCode=$(printf '%x' "${decimalCode}")
    local code="${hexCode}"
    local char="\\U${hexCode}"

    # fill in placeholder cells properly formatted:
    if [ "${char}" = "\\U0" ]; then
      char=" "
      code=""
    fi

    allCodes+="${currentColorCode}${code}\t${reset_color}"
    allChars+="${currentColorChar}${char}\t${reset_color}"
    counter=$((counter + 1))
    count=$(( (count + 1) % wrapAt))

    if [[ $count -eq 0 ]]; then
      printf "%b"  "$allCodes"
      printf "\\n"
      printf "%b"  "$allChars"
      printf "\\n"

      allCodes=""
      allChars=""
    fi

  done
}

function print-unicode-ranges() {
  echo ''

  local arr=("$@")
  local len=$#
  local combinedRanges=()

  for ((j=0; j<len; j+=2)); do
    local start="${arr[$j]}"
    local end="${arr[(($j+1))]}"
    local startDecimal=$((16#$start))
    local endDecimal=$((16#$end))

    # shellcheck disable=SC2207 # We DO WANT the output to be split
    combinedRanges+=($(seq "$startDecimal" "$endDecimal"))

  done

  print-decimal-unicode-range "${combinedRanges[@]}"

}

function test-fonts() {
  echo "Nerd Fonts - Font Power Symbols"
  print-unicode-ranges 23fb 23fe 2b58 2b58
  echo; echo

  echo "Nerd Fonts - Pomicons"
  print-unicode-ranges e000 e00d
  echo; echo

  echo "Nerd Fonts - Powerline"
  print-unicode-ranges e0a0 e0a2 e0b0 e0b3
  echo; echo

  echo "Nerd Fonts - Powerline Extra"
  print-unicode-ranges e0a3 e0a3 e0b4 e0c8 e0ca e0ca e0cc e0d2 e0d4 e0d4
  echo; echo

  echo "Nerd Fonts - Font awesome extension"
  print-unicode-ranges e200 e2a9
  echo; echo


  echo "Nerd Fonts - Weather Icons"
  print-unicode-ranges e300 e3eb
  echo; echo

  echo "Nerd Fonts - Symbols original"
  print-unicode-ranges e5fa e62b
  echo; echo

  echo "Nerd Fonts - Devicons"
  print-unicode-ranges e700 e7c5
  echo; echo

  echo "Nerd Fonts - Codeicons"
  print-unicode-ranges ea60 ebeb
  echo; echo

  echo "Nerd Fonts - Font awesome"
  print-unicode-ranges f000 f2e0
  echo; echo

  echo "Nerd Fonts - Octicons"
  print-unicode-ranges 2665 2665
  print-unicode-ranges 26A1 26A1
  print-unicode-ranges f27c f27c
  print-unicode-ranges f400 f532
  echo; echo

  echo "Nerd Fonts - Font Logos"
  print-unicode-ranges f300 f32f
  echo; echo

  echo "Nerd Fonts - Material Design Icons"
  print-unicode-ranges f500 f533
  echo; echo

  echo "Nerd Fonts - Material Design Icons"
  print-unicode-ranges f0000 f1af0
  echo; echo

}

wrappingValue="$1"

test-fonts
