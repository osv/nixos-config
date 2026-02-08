set -euo pipefail

SHOW_ALL=0
FULL_PATH=0
for arg in "$@"; do
  case "$arg" in
    --all) SHOW_ALL=1 ;;
    --full|-f)  FULL_PATH=1 ;;
    --help|-h)
      echo "Find dead (unconfigured) entries in /persist."
      echo ""
      echo "On tmpfs-root NixOS systems, all persistent data lives under"
      echo "/persist/{state,derivative,cache}. Over time, stale entries may"
      echo "accumulate from removed modules, manual files, etc. This tool"
      echo "compares the actual contents of /persist against the paths declared"
      echo "in nerv.opt.persist.* and reports anything not covered."
      echo ""
      echo "Usage: my-show-dead-but-persisted [--all] [--full|-f]"
      echo ""
      echo "Options:"
      echo "  --all       Show managed (known) entries too"
      echo "  --full, -f  Print only full paths of dead entries (for scripting)"
      echo ""
      echo "Examples:"
      echo "  my-show-dead-but-persisted              # tree view with sizes"
      echo "  my-show-dead-but-persisted --full        # plain list for piping"
      echo "  my-show-dead-but-persisted -f | xargs rm -rf  # delete all dead"
      exit 0
      ;;
    *)
      echo "Unknown option: $arg" >&2
      exit 1
      ;;
  esac
done

# Load known paths into associative array
declare -A known_paths
while IFS= read -r line; do
  [[ -n "$line" ]] && known_paths["$line"]=1
done < @KNOWN_PATHS_FILE@

# Load parent prefixes into associative array
declare -A parent_prefixes
while IFS= read -r line; do
  [[ -n "$line" ]] && parent_prefixes["$line"]=1
done < @PARENT_PREFIXES_FILE@

RED='\033[0;31m'
GRAY='\033[0;90m'
GREEN='\033[0;32m'
BOLD='\033[1m'
NC='\033[0m'

DEAD_COUNT=0
PERSIST_ROOT="@PERSIST_ROOT@"

# Tree drawing characters
T_BRANCH=$'\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80' # ├──
T_LAST=$'\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80'    # └──
T_PIPE=$'\xe2\x94\x82  '                            # │  (with trailing spaces)
T_SPACE='   '                                        # 3 spaces

walk() {
  local dir="$1"
  local prefix="$2"

  # Collect entries
  local entries=()
  while IFS= read -r -d "" entry; do
    entries+=("$entry")
  done < <(find "$dir" -maxdepth 1 -mindepth 1 -print0 2>/dev/null | sort -z)

  local total=${#entries[@]}
  local i=0

  for entry in "${entries[@]}"; do
    i=$((i + 1))
    local name
    name="$(basename "$entry")"
    local is_last=0
    [[ "$i" -eq "$total" ]] && is_last=1

    local connector="$T_BRANCH"
    local child_prefix="${prefix}${T_PIPE}"
    if [[ "$is_last" -eq 1 ]]; then
      connector="$T_LAST"
      child_prefix="${prefix}${T_SPACE}"
    fi

    if [[ -n "${known_paths[$entry]+x}" ]]; then
      # Known/managed path
      if [[ "$SHOW_ALL" -eq 1 ]]; then
        local suffix=""
        [[ -d "$entry" ]] && suffix="/"
        echo -e "${prefix}${connector} ${GREEN}${name}${suffix}${NC} (managed)"
      fi
    elif [[ -n "${parent_prefixes[$entry]+x}" ]]; then
      # Parent prefix — show as context and recurse
      echo -e "${prefix}${connector} ${GRAY}${name}/${NC}"
      walk "$entry" "$child_prefix"
    else
      # Dead entry
      local suffix=""
      [[ -d "$entry" ]] && suffix="/"
      local size
      size="$(du -sh "$entry" 2>/dev/null | cut -f1)" || size="?"

      echo -e "${prefix}${connector} ${RED}${BOLD}${name}${suffix}${NC} ${RED}[$size] DEAD${NC}"
      DEAD_COUNT=$((DEAD_COUNT + 1))
    fi
  done
}

walk_plain() {
  local dir="$1"
  while IFS= read -r -d "" entry; do
    if [[ -n "${known_paths[$entry]+x}" ]]; then
      :
    elif [[ -n "${parent_prefixes[$entry]+x}" ]]; then
      walk_plain "$entry"
    else
      echo "$entry"
    fi
  done < <(find "$dir" -maxdepth 1 -mindepth 1 -print0 2>/dev/null | sort -z)
}

if [[ "$FULL_PATH" -eq 1 ]]; then
  for kind in state derivative cache; do
    kind_dir="$PERSIST_ROOT/$kind"
    [[ -d "$kind_dir" ]] && walk_plain "$kind_dir"
  done
else
  echo "Scanning $PERSIST_ROOT for dead (unconfigured) entries..."
  echo ""

  for kind in state derivative cache; do
    kind_dir="$PERSIST_ROOT/$kind"
    echo -e "${BOLD}=== $kind ===${NC}"
    if [[ ! -d "$kind_dir" ]]; then
      echo "  (not found)"
    else
      if find "$kind_dir" -maxdepth 1 -mindepth 1 -print0 2>/dev/null | read -r -d "" _; then
        walk "$kind_dir" ""
      else
        echo "  (clean)"
      fi
    fi
    echo ""
  done

  if [[ "$DEAD_COUNT" -gt 0 ]]; then
    echo -e "${RED}Found $DEAD_COUNT dead entry(ies).${NC}"
    echo "Review and delete manually if no longer needed."
  else
    echo -e "${GREEN}No dead entries found. Everything is clean!${NC}"
  fi
fi
