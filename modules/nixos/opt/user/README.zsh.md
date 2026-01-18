# ZSH Configuration and Commands Reference

This document provides a comprehensive list of commands and functionality available in the ZSH setup configured in this NixOS system.

## Shell Configuration

### Core ZSH Features
- **Autosuggestions**: Enabled - provides inline suggestions based on command history
- **Syntax Highlighting**: Enabled - highlights valid/invalid commands as you type
- **Auto-completion**: Enabled via `carapace` - advanced command completion
- **History**: 30,000 commands stored in `$XDG_DATA_HOME/zsh/zsh_history`
  - Ignores duplicates and commands starting with spaces
  - Shares history between sessions

### Key Bindings
- `Ctrl+[` or `Home`: Go to beginning of line
- `Ctrl+]` or `End`: Go to end of line
- `Up Arrow`: History search backward from current input
- `Down Arrow`: History search forward from current input
- `Ctrl+Left`: Move backward by word
- `Ctrl+Right`: Move forward by word
- `Ctrl+G`: **fzf-marks** - Jump to bookmarked directories
- `Ctrl+X /`: Force file path completion (same as Emacs)

## Available Commands by Category

### File and Directory Navigation

#### Aliases (eza-based)
- `exa` → `eza --group-directories-first --git`
- `l` → `eza -lbF --icons` (long format with icons)
- `ll` → `eza -lbGF --icons` (long format with Git status and icons)
- `lla` → `eza -labGF --icons` (all files in long format)
- `llm` → `ll --sort=modified` (sorted by modification time)
- `la` → `LC_COLLATE=C eza -ablF --icons` (all files)
- `tree` → `eza --tree` (tree view)

#### Aliases (colorls-based)
- `lc` → `colorls --sd` (sort directories first)
- `lcg` → `lc --gs` (with Git status)
- `lcl` → `lc -1` (single column)
- `lclg` → `lc -1 --gs` (single column with Git status)
- `lcu` → `colorls -U` (unsorted)
- `lclu` → `lcu -U -1` (unsorted, single column)

### Directory Bookmarks (fzf-marks plugin)
- `mark <name>` - Bookmark current directory with given name
- `fzm [query]` - Interactive bookmark selector with fzf
  - `Ctrl+Y`: Jump to selected bookmark
  - `Ctrl+T`: Toggle bookmark for deletion
  - `Ctrl+D`: Delete selected bookmarks
- `Ctrl+G` - Quick access to `fzf` bookmark selector

### Git Commands (forgit plugin)

#### Interactive Git Operations
- `ga` - Interactive git add (select files to stage)
- `glo` - Interactive git log viewer
- `gd` - Interactive git diff viewer
- `gso` - Interactive git show viewer (commit details)
- `grh` - Interactive git reset HEAD (unstage files)
- `gcf` - Interactive git checkout file (restore files)
- `gcb` - Interactive git checkout branch
- `gbd` - Interactive git branch delete
- `gct` - Interactive git checkout tag
- `gco` - Interactive git checkout commit
- `grc` - Interactive git revert commit
- `gss` - Interactive git stash show
- `gsp` - Interactive git stash push
- `gclean` - Interactive git clean (remove untracked files)
- `gcp` - Interactive git cherry-pick
- `grb` - Interactive git rebase selector
- `gbl` - Interactive git blame viewer
- `gfu` - Interactive git fixup
- `gsq` - Interactive git squash
- `grw` - Interactive git reword

### System Monitoring and Logs

#### Journal Aliases
- `j` - `journalctl --no-hostname -eb -o short-monotonic` (system journal)
- `jw` - `j -p notice` (journal warnings and above)
- `jf` - `j -f` (follow journal in real-time)
- `jk` - `j -k` (journal kernel messages)
- `jhome` - `j -u home-manager-osv.service` (home-manager service logs)

### Search and File Operations
- `bat` - Syntax-highlighted file viewer (with `base16` theme)
- `fd` - Fast file finder (modern `find` replacement)
- `ripgrep` / `rg` - Fast text search tool
- `fzf` - Fuzzy finder for interactive selection
- `tlrc` - tldr client for practical command examples

### Text Processing and Fun
- `grc <command>` - Colorize output of commands (ping, mount, df, etc.)
- `cowsay <text>` - ASCII art cow with speech bubble
- `fortune` - Random fortune/quote (runs automatically on shell start)
- `lolcat` - Rainbow colorize text output
- `r <time> <message>` - Set a reminder (e.g., `r 15m "Take a break"`)

### Auto-pairing (zsh-autopair plugin)
Automatically inserts and manages matching pairs:
- Quotes: `"` `'` `` ` ``
- Brackets: `()` `[]` `{}`
- Automatically skips over existing pairs
- Smart deletion of pairs when backspacing
- Expands spaces between brackets

### Prompt and Environment
- **Starship**: Modern, customizable prompt with Git integration
- **Carapace**: Advanced auto-completion for 400+ CLI tools
- **Term Title Support**: Updates terminal title based on current command/directory

## Plugin Sources and Configuration

### Installed Plugins
1. **fzf-marks** (urbainvaes/fzf-marks)
   - Directory bookmarking with fzf integration
   - Bookmarks stored in `$XDG_DATA_HOME/zsh/fzf-marks`

2. **forgit** (wfxr/forgit)
   - Interactive Git operations using fzf
   - All Git operations become visual and interactive

3. **zsh-autopair** (hlissner/zsh-autopair)
   - Intelligent bracket/quote pairing
   - Context-aware auto-completion

4. **zsh-nix-shell** (chisui/zsh-nix-shell)
   - Better Nix shell integration
   - Enables ZSH as default shell in `nix-shell`

5. **term-title-support** (ohmyzsh/ohmyzsh)
   - Dynamic terminal title updates
   - Shows current command and directory

### Environment Variables
- `BAT_THEME=base16` - Sets bat syntax highlighting theme
- `KEYTIMEOUT=1` - Faster key timeout (fixes tmux issues)
- `FZF_MARKS_FILE="$XDG_DATA_HOME/zsh/fzf-marks"` - Bookmark storage location

### History Configuration
- Size: 30,000 commands
- Location: `$XDG_DATA_HOME/zsh/zsh_history`
- Ignores space-prefixed commands (prefix with space to hide from history)
- Removes all duplicate entries, keeping only the latest
- Removes duplicates first when history is full
- Extended history format with timestamps
- Shared between all ZSH sessions

### Word Selection
- Configured to stop on directory delimiters (like bash)
- `Ctrl+W` deletes words stopping at `/`, not entire paths

## Persistence
History and bookmarks are persisted in:
- `~/.local/share/zsh/` - ZSH history and fzf-marks data

This directory is automatically backed up in the NixOS impermanence setup.