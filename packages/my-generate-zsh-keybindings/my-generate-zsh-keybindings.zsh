#!@zsh@
# Generate interactive HTML page with Zsh keybindings
# Reads bindkey -L output, maps widgets to categories/descriptions,
# and inserts JS data into the shared keybinding template.

set -euo pipefail

TEMPLATE="@templatePath@"
INDEX_GENERATOR="@indexGenerator@"
XDG_OPEN="@xdgOpen@"

KEYBINDING_DIR="$HOME/.cache/nixos-config/keybinding"
OUTPUT_DIR="$KEYBINDING_DIR/zsh"
OUTPUT_FILE="$OUTPUT_DIR/index.html"

SILENT=false
[[ "${1:-}" == "--silent" ]] && SILENT=true

# --- Widget descriptions and categories ---
# Format: widget_name -> "category|description"
typeset -A widget_info
# Navigation
widget_info[beginning-of-line]="navigation|Go to beginning of line"
widget_info[end-of-line]="navigation|Go to end of line"
widget_info[forward-char]="navigation|Move forward one character"
widget_info[backward-char]="navigation|Move backward one character"
widget_info[forward-word]="navigation|Move forward one word"
widget_info[backward-word]="navigation|Move backward one word"
widget_info[vi-forward-word]="navigation|Move forward one word (vi-style)"
widget_info[vi-forward-word-end]="navigation|Move to end of word (vi-style)"
widget_info[vi-backward-word]="navigation|Move backward one word (vi-style)"
widget_info[emacs-forward-word]="navigation|Move forward one word (emacs-style)"
widget_info[emacs-backward-word]="navigation|Move backward one word (emacs-style)"
widget_info[vi-find-next-char]="navigation|Find next character"
widget_info[vi-find-prev-char]="navigation|Find previous character"
widget_info[vi-goto-column]="navigation|Go to column"
widget_info[vi-first-non-blank]="navigation|Go to first non-blank character"
widget_info[vi-end-of-line]="navigation|Go to end of line (vi-style)"
widget_info[vi-forward-blank-word]="navigation|Move forward one WORD"
widget_info[vi-forward-blank-word-end]="navigation|Move to end of WORD"
widget_info[vi-backward-blank-word]="navigation|Move backward one WORD"
widget_info[vi-match-bracket]="navigation|Go to matching bracket"
widget_info[vi-repeat-find]="navigation|Repeat character find"
widget_info[vi-rev-repeat-find]="navigation|Reverse repeat character find"

# History
widget_info[up-line-or-history]="history|Move up in history or multi-line"
widget_info[down-line-or-history]="history|Move down in history or multi-line"
widget_info[up-history]="history|Previous command in history"
widget_info[down-history]="history|Next command in history"
widget_info[history-search-backward]="history|Search backward in history"
widget_info[history-search-forward]="history|Search forward in history"
widget_info[history-beginning-search-backward]="history|Search backward from current input"
widget_info[history-beginning-search-forward]="history|Search forward from current input"
widget_info[beginning-of-buffer-or-history]="history|Go to first line of history"
widget_info[end-of-buffer-or-history]="history|Go to last line of history"
widget_info[history-incremental-search-backward]="history|Incremental search backward (Ctrl+R)"
widget_info[history-incremental-search-forward]="history|Incremental search forward (Ctrl+S)"
widget_info[accept-line-and-down-history]="history|Accept line and show next history"
widget_info[infer-next-history]="history|Infer next history line"
widget_info[vi-fetch-history]="history|Fetch history by number"
widget_info[up-line-or-beginning-search]="history|Move up or search from beginning"
widget_info[down-line-or-beginning-search]="history|Move down or search from beginning"

# Editing
widget_info[kill-line]="editing|Kill from cursor to end of line"
widget_info[backward-kill-line]="editing|Kill from cursor to beginning of line"
widget_info[kill-whole-line]="editing|Kill entire line"
widget_info[kill-word]="editing|Kill word forward"
widget_info[backward-kill-word]="editing|Kill word backward"
widget_info[kill-buffer]="editing|Kill entire buffer"
widget_info[backward-delete-char]="editing|Delete character backward"
widget_info[delete-char]="editing|Delete character forward"
widget_info[delete-char-or-list]="editing|Delete char or list completions"
widget_info[delete-word]="editing|Delete word forward"
widget_info[backward-delete-word]="editing|Delete word backward"
widget_info[yank]="editing|Yank (paste) killed text"
widget_info[yank-pop]="editing|Cycle through kill ring"
widget_info[quoted-insert]="editing|Insert next character literally"
widget_info[transpose-chars]="editing|Transpose characters"
widget_info[transpose-words]="editing|Transpose words"
widget_info[capitalize-word]="editing|Capitalize word"
widget_info[up-case-word]="editing|Uppercase word"
widget_info[down-case-word]="editing|Lowercase word"
widget_info[undo]="editing|Undo last change"
widget_info[redo]="editing|Redo last undone change"
widget_info[overwrite-mode]="editing|Toggle overwrite mode"
widget_info[vi-change]="editing|Change (vi c)"
widget_info[vi-change-eol]="editing|Change to end of line (vi C)"
widget_info[vi-change-whole-line]="editing|Change entire line (vi cc)"
widget_info[vi-delete]="editing|Delete (vi d)"
widget_info[vi-delete-char]="editing|Delete character under cursor (vi x)"
widget_info[vi-backward-delete-char]="editing|Delete character before cursor (vi X)"
widget_info[vi-kill-line]="editing|Kill to beginning of line (vi)"
widget_info[vi-kill-eol]="editing|Kill to end of line (vi)"
widget_info[vi-put-after]="editing|Put after cursor (vi p)"
widget_info[vi-put-before]="editing|Put before cursor (vi P)"
widget_info[vi-yank]="editing|Yank selection (vi y)"
widget_info[vi-yank-whole-line]="editing|Yank entire line (vi yy)"
widget_info[vi-yank-eol]="editing|Yank to end of line (vi Y)"
widget_info[vi-replace]="editing|Replace character (vi r)"
widget_info[vi-replace-chars]="editing|Replace mode (vi R)"
widget_info[vi-substitute]="editing|Substitute (vi s)"
widget_info[vi-swap-case]="editing|Toggle case (vi ~)"
widget_info[vi-indent]="editing|Indent (vi >)"
widget_info[vi-unindent]="editing|Unindent (vi <)"
widget_info[vi-join]="editing|Join lines (vi J)"
widget_info[vi-open-line-above]="editing|Open line above (vi O)"
widget_info[vi-open-line-below]="editing|Open line below (vi o)"
widget_info[vi-add-eol]="editing|Insert at end of line (vi A)"
widget_info[vi-add-next]="editing|Insert after cursor (vi a)"
widget_info[vi-insert]="editing|Enter insert mode (vi i)"
widget_info[vi-insert-bol]="editing|Insert at beginning of line (vi I)"
widget_info[vi-repeat-change]="editing|Repeat last change (vi .)"
widget_info[vi-oper-swap-case]="editing|Swap case of selection"
widget_info[insert-last-word]="editing|Insert last word from previous command"
widget_info[quote-line]="editing|Quote entire line"
widget_info[quote-region]="editing|Quote region between mark and cursor"
widget_info[copy-region-as-kill]="editing|Copy region to kill ring"
widget_info[copy-prev-word]="editing|Copy previous word"
widget_info[autopair-insert]="editing|Auto-insert matching pair"
widget_info[autopair-close]="editing|Auto-close matching pair"
widget_info[autopair-delete]="editing|Delete character backward (autopair)"
widget_info[autopair-delete-word]="editing|Delete word backward (autopair)"
widget_info[set-mark-command]="editing|Set mark at cursor"
widget_info[exchange-point-and-mark]="editing|Swap cursor and mark"
widget_info[kill-region]="editing|Kill region between mark and cursor"
widget_info[vi-set-buffer]="editing|Set named buffer (vi \")"

# Completion
widget_info[expand-or-complete]="completion|Expand alias or complete"
widget_info[expand-or-complete-prefix]="completion|Complete with prefix"
widget_info[complete-word]="completion|Complete current word"
widget_info[menu-complete]="completion|Cycle through completions forward"
widget_info[reverse-menu-complete]="completion|Cycle through completions backward"
widget_info[menu-select]="completion|Select from completion menu"
widget_info[list-choices]="completion|List possible completions"
widget_info[list-expand]="completion|List expansion of current word"
widget_info[expand-word]="completion|Expand current word"
widget_info[expand-history]="completion|Expand history references"
widget_info[magic-space]="completion|Expand history and insert space"
widget_info[accept-and-menu-complete]="completion|Accept and show next completion"
widget_info[complete-files]="completion|Force file path completion"
widget_info[_complete_help]="completion|Show completion system info"
widget_info[_correct_word]="completion|Correct current word"
widget_info[_expand_alias]="completion|Expand alias under cursor"
widget_info[_history-complete-newer]="completion|Complete from newer history"
widget_info[_history-complete-older]="completion|Complete from older history"
widget_info[_most_recent_file]="completion|Complete most recent file"
widget_info[_next_tags]="completion|Next completion tag group"
widget_info[_read_comp]="completion|Read completion specification"
widget_info[_correct_filename]="completion|Correct filename under cursor"
widget_info[_expand_word]="completion|Expand word under cursor"
widget_info[_list_expansions]="completion|List possible expansions"
widget_info[_complete_debug]="completion|Debug completion system"
widget_info[_complete_tag]="completion|Complete tag"
widget_info[_bash_complete-word]="completion|Bash-style word completion"
widget_info[_bash_list-choices]="completion|Bash-style list choices"

# Plugins (fzf, fzf-marks, custom widgets)
widget_info[fzf-file-widget]="plugins|FZF: Find files (Ctrl+T)"
widget_info[fzf-cd-widget]="plugins|FZF: Change directory (Alt+C)"
widget_info[fzf-history-widget]="plugins|FZF: Search history (Ctrl+R)"
widget_info[fzm]="plugins|FZF-marks: Jump to bookmark"
widget_info[fzf-completion]="plugins|FZF: Trigger completion (Tab)"
widget_info[my-export-zsh-keybindings]="plugins|Export keybindings to HTML"

# Misc
widget_info[clear-screen]="misc|Clear screen"
widget_info[reset-prompt]="misc|Reset prompt"
widget_info[redisplay]="misc|Redisplay prompt"
widget_info[push-line]="misc|Push line to stack"
widget_info[push-line-or-edit]="misc|Push line or edit multi-line"
widget_info[get-line]="misc|Pop line from stack"
widget_info[push-input]="misc|Push input to stack"
widget_info[run-help]="misc|Show help for command"
widget_info[execute-named-cmd]="misc|Execute named ZLE command"
widget_info[execute-last-named-cmd]="misc|Repeat last named command"
widget_info[vi-cmd-mode]="misc|Enter vi command mode"
widget_info[vi-pound-insert]="misc|Comment out line (vi #)"
widget_info[vi-digit-or-beginning-of-line]="misc|Digit argument or go to BOL"
widget_info[accept-line]="misc|Accept and execute line"
widget_info[spell-word]="misc|Spell-check current word"
widget_info[which-command]="misc|Show which command"
widget_info[where-is]="misc|Show key binding for widget"
widget_info[describe-key-briefly]="misc|Describe what a key does"
widget_info[select-a-word]="misc|Select a word (text object)"
widget_info[select-in-word]="misc|Select in word (text object)"
widget_info[select-a-blank-word]="misc|Select a WORD (text object)"
widget_info[select-in-blank-word]="misc|Select in WORD (text object)"
widget_info[select-a-shell-word]="misc|Select a shell word (text object)"
widget_info[select-in-shell-word]="misc|Select in shell word (text object)"
widget_info[accept-and-hold]="misc|Accept line and keep in edit buffer"
widget_info[what-cursor-position]="misc|Show cursor position information"
widget_info[bracketed-paste]="misc|Bracketed paste mode"
widget_info[send-break]="misc|Abort current command"
widget_info[pound-insert]="misc|Comment/uncomment line"
widget_info[argument-base]="misc|Set argument base"
widget_info[recursive-edit]="misc|Enter recursive edit"
widget_info[vi-set-mark]="misc|Set vi mark"
widget_info[vi-goto-mark]="misc|Go to vi mark"
widget_info[vi-goto-mark-line]="misc|Go to vi mark line"
widget_info[vi-undo-change]="misc|Undo change (vi u)"

# Widgets to ignore (too noisy / not useful)
typeset -A ignore_widgets
ignore_widgets[self-insert]=1
ignore_widgets[undefined-key]=1
ignore_widgets[digit-argument]=1
ignore_widgets[neg-argument]=1
ignore_widgets[beep]=1
ignore_widgets[vi-digit-or-beginning-of-line]=1
ignore_widgets[self-insert-unmeta]=1
ignore_widgets[magic-space]=1
ignore_widgets[bracketed-paste]=1

# --- Escape sequence conversion ---
convert_key() {
  local seq="$1"

  # ^? → Backspace
  [[ "$seq" == '^?' ]] && echo '<Backspace>' && return

  # ^[ alone → Escape
  [[ "$seq" == '^[' ]] && echo 'Escape' && return

  # ^M → Enter, ^I → Tab, ^J → Enter
  [[ "$seq" == '^M' ]] && echo 'Enter' && return
  [[ "$seq" == '^I' ]] && echo 'Tab' && return
  [[ "$seq" == '^J' ]] && echo 'Enter' && return

  # ^X^<letter> → chord C-x C-<letter>
  if [[ "$seq" =~ '^\^X\^([A-Z])$' ]]; then
    local ch="${match[1]}"
    ch="${ch:l}"
    echo "C-x C-${ch}"
    return
  fi

  # ^X<char> → chord C-x <char> (preserve case for Shift distinction)
  if [[ "$seq" =~ '^\^X(.)$' ]]; then
    local ch="${match[1]}"
    echo "C-x ${ch}"
    return
  fi

  # ^<letter> → C-<letter>
  if [[ "$seq" =~ '^\^([]A-Z_\\@[])$' ]]; then
    local letter="${match[1]}"
    letter="${letter:l}" # lowercase
    echo "C-${letter}"
    return
  fi

  # ^[<letter> → M-<letter> (Alt+letter)
  if [[ "$seq" =~ '^\^\[([a-zA-Z0-9])$' ]]; then
    local ch="${match[1]}"
    ch="${ch:l}" # lowercase
    echo "M-${ch}"
    return
  fi

  # ^[^<letter> → C-M-<letter>
  if [[ "$seq" =~ '^\^\[\^([A-Z])$' ]]; then
    local ch="${match[1]}"
    ch="${ch:l}"
    echo "C-M-${ch}"
    return
  fi

  # SS3 keys: ^[OP=F1..^[OS=F4, ^[OA=Up..^[OD=Left, ^[OH=Home, ^[OF=End
  if [[ "$seq" =~ '^\^\[O([PQRSABCDHF])$' ]]; then
    local fkey
    case "${match[1]}" in
      P) fkey="F1" ;; Q) fkey="F2" ;; R) fkey="F3" ;; S) fkey="F4" ;;
      A) fkey="Up" ;; B) fkey="Down" ;; C) fkey="Right" ;; D) fkey="Left" ;;
      H) fkey="Home" ;; F) fkey="End" ;;
    esac
    echo "<${fkey}>"
    return
  fi

  # CSI modified SS3 keys: ^[[1;<mod>P = modified F1, etc.
  if [[ "$seq" =~ '^\^\[\[1;([0-9]+)([PQRS])$' ]]; then
    local mod="${match[1]}"
    local fkey
    case "${match[2]}" in
      P) fkey="F1" ;;
      Q) fkey="F2" ;;
      R) fkey="F3" ;;
      S) fkey="F4" ;;
    esac
    local prefix=""
    prefix=$(decode_modifier "$mod")
    echo "${prefix}<${fkey}>"
    return
  fi

  # CSI function keys: ^[[15~=F5, ^[[17~=F6, ^[[18~=F7, ^[[19~=F8
  # ^[[20~=F9, ^[[21~=F10, ^[[23~=F11, ^[[24~=F12
  if [[ "$seq" =~ '^\^\[\[([0-9]+)~$' ]]; then
    local num="${match[1]}"
    local key=""
    case "$num" in
      2) key="Insert" ;;
      3) key="Delete" ;;
      5) key="PageUp" ;;
      6) key="PageDown" ;;
      15) key="F5" ;;
      17) key="F6" ;;
      18) key="F7" ;;
      19) key="F8" ;;
      20) key="F9" ;;
      21) key="F10" ;;
      23) key="F11" ;;
      24) key="F12" ;;
      *) key="Unknown-$num" ;;
    esac
    echo "<${key}>"
    return
  fi

  # CSI modified function keys: ^[[<num>;<mod>~
  if [[ "$seq" =~ '^\^\[\[([0-9]+);([0-9]+)~$' ]]; then
    local num="${match[1]}"
    local mod="${match[2]}"
    local key=""
    case "$num" in
      2) key="Insert" ;;
      3) key="Delete" ;;
      5) key="PageUp" ;;
      6) key="PageDown" ;;
      15) key="F5" ;;
      17) key="F6" ;;
      18) key="F7" ;;
      19) key="F8" ;;
      20) key="F9" ;;
      21) key="F10" ;;
      23) key="F11" ;;
      24) key="F12" ;;
      *) key="Unknown-$num" ;;
    esac
    local prefix=""
    prefix=$(decode_modifier "$mod")
    echo "${prefix}<${key}>"
    return
  fi

  # Arrow keys: ^[[A=Up, ^[[B=Down, ^[[C=Right, ^[[D=Left
  if [[ "$seq" =~ '^\^\[\[([ABCDHF])$' ]]; then
    local key=""
    case "${match[1]}" in
      A) key="Up" ;;
      B) key="Down" ;;
      C) key="Right" ;;
      D) key="Left" ;;
      H) key="Home" ;;
      F) key="End" ;;
    esac
    echo "<${key}>"
    return
  fi

  # Modified arrows/home/end: ^[[1;<mod><letter>
  if [[ "$seq" =~ '^\^\[\[1;([0-9]+)([ABCDHF])$' ]]; then
    local mod="${match[1]}"
    local key=""
    case "${match[2]}" in
      A) key="Up" ;;
      B) key="Down" ;;
      C) key="Right" ;;
      D) key="Left" ;;
      H) key="Home" ;;
      F) key="End" ;;
    esac
    local prefix=""
    prefix=$(decode_modifier "$mod")
    echo "${prefix}<${key}>"
    return
  fi

  # Shift-Tab: ^[[Z
  [[ "$seq" == '^[[Z' ]] && echo 'S-<Tab>' && return

  # ^[^? → M-<Backspace> (Alt+Backspace)
  [[ "$seq" == '^[^?' ]] && echo 'M-<Backspace>' && return

  # ^[^_ → C-M-_ (Ctrl+Alt+Underscore)
  [[ "$seq" == '^[^_' ]] && echo 'C-M-_' && return

  # ^[<special> sequences like ^[. ^[, etc
  if [[ "$seq" =~ '^\^\[(.+)$' ]]; then
    local rest="${match[1]}"
    # Recursively handle the rest
    echo "M-${rest}"
    return
  fi

  # Fallback: return raw sequence
  echo "$seq"
}

decode_modifier() {
  local mod="$1"
  case "$mod" in
    2) echo "S-" ;;      # Shift
    3) echo "M-" ;;      # Alt
    4) echo "S-M-" ;;    # Shift+Alt
    5) echo "C-" ;;      # Ctrl
    6) echo "C-S-" ;;    # Ctrl+Shift
    7) echo "C-M-" ;;    # Ctrl+Alt
    8) echo "C-S-M-" ;;  # Ctrl+Shift+Alt
    *) echo "" ;;
  esac
}

# --- Test mode ---
if [[ "${1:-}" == "--test" ]]; then
    local fail=0 pass=0 total=0

    assert_convert() {
        total=$((total + 1))
        local input="$1" expected="$2"
        local actual
        actual=$(convert_key "$input")
        if [[ "$actual" == "$expected" ]]; then
            pass=$((pass + 1))
        else
            fail=$((fail + 1))
            echo "FAIL: convert_key '$input' = '$actual' (expected '$expected')"
        fi
    }

    # ^<letter> → C-<letter>
    assert_convert '^A' 'C-a'
    assert_convert '^Z' 'C-z'
    assert_convert '^@' 'C-@'
    assert_convert '^_' 'C-_'
    assert_convert '^\' 'C-\'

    # Special single chars
    assert_convert '^?' '<Backspace>'
    assert_convert '^[' 'Escape'
    assert_convert '^M' 'Enter'
    assert_convert '^I' 'Tab'
    assert_convert '^J' 'Enter'

    # ^X^<letter> chords (MUST be before ^X<char> test)
    assert_convert '^X^B' 'C-x C-b'
    assert_convert '^X^F' 'C-x C-f'
    assert_convert '^X^X' 'C-x C-x'

    # ^X<char> chords (preserve case!)
    assert_convert '^Xc' 'C-x c'
    assert_convert '^XC' 'C-x C'
    assert_convert '^X*' 'C-x *'
    assert_convert '^X/' 'C-x /'

    # Alt (^[<letter>)
    assert_convert '^[a' 'M-a'
    assert_convert '^[b' 'M-b'
    assert_convert '^[A' 'M-a'

    # Ctrl+Alt (^[^<letter>)
    assert_convert '^[^D' 'C-M-d'
    assert_convert '^[^G' 'C-M-g'

    # Special Alt combos
    assert_convert '^[^?' 'M-<Backspace>'
    assert_convert '^[^_' 'C-M-_'
    assert_convert '^[|' 'M-|'

    # SS3 function keys
    assert_convert '^[OP' '<F1>'
    assert_convert '^[OQ' '<F2>'
    assert_convert '^[OR' '<F3>'
    assert_convert '^[OS' '<F4>'

    # SS3 arrows/home/end (now handled properly)
    assert_convert '^[OA' '<Up>'
    assert_convert '^[OB' '<Down>'
    assert_convert '^[OC' '<Right>'
    assert_convert '^[OD' '<Left>'
    assert_convert '^[OH' '<Home>'
    assert_convert '^[OF' '<End>'

    # CSI arrows
    assert_convert '^[[A' '<Up>'
    assert_convert '^[[B' '<Down>'
    assert_convert '^[[C' '<Right>'
    assert_convert '^[[D' '<Left>'
    assert_convert '^[[H' '<Home>'
    assert_convert '^[[F' '<End>'

    # Modified arrows
    assert_convert '^[[1;5C' 'C-<Right>'
    assert_convert '^[[1;5D' 'C-<Left>'
    assert_convert '^[[1;3A' 'M-<Up>'

    # Modified function keys (CSI)
    assert_convert '^[[1;4P' 'S-M-<F1>'
    assert_convert '^[[1;5P' 'C-<F1>'

    # CSI special keys
    assert_convert '^[[2~' '<Insert>'
    assert_convert '^[[3~' '<Delete>'
    assert_convert '^[[5~' '<PageUp>'
    assert_convert '^[[6~' '<PageDown>'
    assert_convert '^[[15~' '<F5>'
    assert_convert '^[[24~' '<F12>'

    # Shift-Tab
    assert_convert '^[[Z' 'S-<Tab>'

    # Alt + punctuation fallback
    assert_convert '^[ ' 'M- '
    assert_convert '^[!' 'M-!'

    echo ""
    echo "Results: $pass passed, $fail failed (of $total)"
    [[ $fail -eq 0 ]] && echo "ALL TESTS PASSED" || echo "SOME TESTS FAILED"
    exit $fail
fi

# --- Read bindkey output ---
BINDKEY_FILE="${BINDKEY_FILE:-${XDG_RUNTIME_DIR:-/tmp}/zsh-bindkey-export}"

if [[ ! -f "$BINDKEY_FILE" ]]; then
  echo "Error: bindkey export file not found: $BINDKEY_FILE" >&2
  echo "Run: bindkey -L > $BINDKEY_FILE" >&2
  exit 1
fi

# --- Parse bindkey output ---
typeset -a kb_entries  # array of "combo\tcategory\tdescription"

while IFS= read -r line; do
  # Parse: bindkey "SEQUENCE" WIDGET
  # or: bindkey -R "RANGE" WIDGET (skip ranges)
  [[ "$line" =~ '^bindkey -R ' ]] && continue
  [[ "$line" =~ '^bindkey "(.*)" (.+)$' ]] || continue

  local raw_seq="${match[1]}"
  local widget="${match[2]}"

  # Skip ignored widgets
  (( ${+ignore_widgets[$widget]} )) && continue

  # Skip SS3 arrow/home/end (duplicates of CSI versions)
  [[ "$raw_seq" =~ '^\^\[O[ABCDHF]$' ]] && continue

  # Look up widget info, fallback to misc + widget name
  local category description
  if (( ${+widget_info[$widget]} )); then
    local info="${widget_info[$widget]}"
    category="${info%%|*}"
    description="${info#*|}"
  else
    category="misc"
    description="${widget//-/ }"
  fi

  # Convert escape sequence to combo
  local combo
  combo=$(convert_key "$raw_seq")

  # Skip empty or raw-looking combos
  [[ -z "$combo" ]] && continue

  # Skip autopair widgets bound to single printable characters (noise)
  if [[ "$widget" =~ '^autopair-' && ${#raw_seq} == 1 ]]; then
    continue
  fi

  kb_entries+=("${combo}"$'\t'"${category}"$'\t'"${description}")
done < "$BINDKEY_FILE"

# --- Deduplicate: keep last binding for each combo ---
typeset -A seen_combos
typeset -a deduped_entries

for entry in "${kb_entries[@]}"; do
  local combo="${entry%%$'\t'*}"
  seen_combos[$combo]="$entry"
done

# Preserve order by iterating kb_entries, but only take last seen
typeset -A emitted
for entry in "${kb_entries[@]}"; do
  local combo="${entry%%$'\t'*}"
  local canonical="${seen_combos[$combo]}"
  if (( ! ${+emitted[$combo]} )); then
    deduped_entries+=("$canonical")
    emitted[$combo]=1
  fi
done

# --- Generate JS data ---
mkdir -p "$OUTPUT_DIR"
local tmpfile
tmpfile=$(mktemp)

{
  cat <<'JSHEAD'
const appConfig = {
    title: 'Zsh Keybindings',
    modifiers: { 'C': 'Ctrl', 'M': 'Alt', 'S': 'Shift' }
};
const categories = {
    'navigation': 'Navigation',
    'history': 'History',
    'editing': 'Editing',
    'completion': 'Completion',
    'plugins': 'Plugins',
    'misc': 'Miscellaneous'
};
const keybindings = [
JSHEAD

  for entry in "${deduped_entries[@]}"; do
    local combo="${entry%%$'\t'*}"
    local rest="${entry#*$'\t'}"
    local category="${rest%%$'\t'*}"
    local description="${rest#*$'\t'}"

    # Escape single quotes in description
    description="${description//\'/\\\'}"
    combo="${combo//\'/\\\'}"

    echo "    ['${combo}', '${category}', '${description}'],"
  done

  echo "];"
} > "$tmpfile"

# --- Insert into template ---
# Use awk instead of sed for reliability with multi-line insertion
awk -v datafile="$tmpfile" '
/\/\/ app_keybinding_data/ {
    while ((getline line < datafile) > 0) print line
    close(datafile)
    next
}
{ print }
' "$TEMPLATE" > "$OUTPUT_FILE"

rm -f "$tmpfile"

echo "Generated: $OUTPUT_FILE"
echo "Keybindings: ${#deduped_entries[@]}"

# --- Update main index ---
"$INDEX_GENERATOR"

# --- Open in browser ---
if [[ "$SILENT" == false ]]; then
  "$XDG_OPEN" "$OUTPUT_FILE" &>/dev/null &
  disown
fi
