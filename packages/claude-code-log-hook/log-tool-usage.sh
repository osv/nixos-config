#!/usr/bin/env bash

# Claude Code Tool Usage Logger Hook
# Logs all tool usage to /tmp/claude-code-log/<project>/<session_id>.md

  # "hooks": {
  #   "PreToolUse": [
  #     {
  #       "matcher": ".*",
  #       "hooks": [
  #         {
  #           "type": "command",
  #           "command": "/home/osv/work/my/nixos-config/packages/claude-code-log-hook/log-tool-usage.sh pre"
  #         }
  #       ]
  #     }
  #   ],
  #   "PostToolUse": [
  #     {
  #       "matcher": ".*",
  #       "hooks": [
  #         {
  #           "type": "command",
  #           "command": "/home/osv/work/my/nixos-config/packages/claude-code-log-hook/log-tool-usage.sh post"
  #         }
  #       ]
  #     }
  #   ]
  # },

set -euo pipefail

# Read hook type from command line argument (pre/post)
HOOK_TYPE="${1:-}"

# Read JSON input from stdin
JSON_INPUT=$(cat)

# Extract common fields
SESSION_ID=$(echo "$JSON_INPUT" | jq -r '.session_id // "unknown"')
TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')

# Get project name from environment or cwd
if [ -n "${CLAUDE_PROJECT_DIR:-}" ]; then
    PROJECT_NAME=$(basename "$CLAUDE_PROJECT_DIR")
else
    CWD=$(echo "$JSON_INPUT" | jq -r '.cwd // "."')
    PROJECT_NAME=$(basename "$CWD")
fi

# Sanitize project name and session ID for filesystem
PROJECT_NAME=$(echo "$PROJECT_NAME" | tr '/' '_' | tr ' ' '_')
SESSION_ID=$(echo "$SESSION_ID" | tr '/' '_' | tr ' ' '_')

# Create log directory structure
LOG_DIR="/tmp/claude-code-log/${PROJECT_NAME}"
mkdir -p "$LOG_DIR"

# Log file path
LOG_FILE="${LOG_DIR}/${SESSION_ID}.md"

# Function to format JSON values in markdown
format_json_value() {
    local value="$1"
    # Check if it's a simple value or complex object/array
    if echo "$value" | jq -e 'type == "object" or type == "array"' >/dev/null 2>&1; then
        echo '```json'
        echo "$value" | jq '.'
        echo '```'
    else
        # For simple values, remove quotes if it's a string
        echo '```'
        echo "$value" | jq -r '.'
        echo '```'
    fi
}

# Handle PreToolUse hook
if [ "$HOOK_TYPE" = "pre" ]; then
    TOOL_NAME=$(echo "$JSON_INPUT" | jq -r '.tool_name // "unknown"')
    TOOL_INPUT=$(echo "$JSON_INPUT" | jq '.tool_input // {}')
    
    {
        echo ""
        echo "# [$TIMESTAMP] PRE-TOOL: $TOOL_NAME"
        echo "Request:"
        
        # Iterate through each key-value pair in tool_input
        echo "$TOOL_INPUT" | jq -r 'to_entries[] | .key' | while read -r key; do
            value=$(echo "$TOOL_INPUT" | jq ".\"$key\"")
            echo "${key}:"
            format_json_value "$value"
        done
        
    } >> "$LOG_FILE"
    
# Handle PostToolUse hook
elif [ "$HOOK_TYPE" = "post" ]; then
    TOOL_NAME=$(echo "$JSON_INPUT" | jq -r '.tool_name // "unknown"')
    TOOL_OUTPUT=$(echo "$JSON_INPUT" | jq '.tool_output // {}')
    EXIT_CODE=$(echo "$JSON_INPUT" | jq -r '.exit_code // 0')
    
    {
        echo ""
        echo "# [$TIMESTAMP] POST-TOOL: $TOOL_NAME"
        echo "Response (exit code: $EXIT_CODE):"
        
        # For tool output, it might be a string or object
        if echo "$TOOL_OUTPUT" | jq -e 'type == "string"' >/dev/null 2>&1; then
            echo "output:"
            echo '```'
            echo "$TOOL_OUTPUT" | jq -r '.'
            echo '```'
        else
            # If it's an object, iterate through keys
            echo "$TOOL_OUTPUT" | jq -r 'to_entries[] | .key' 2>/dev/null | while read -r key; do
                value=$(echo "$TOOL_OUTPUT" | jq ".\"$key\"")
                echo "${key}:"
                format_json_value "$value"
            done || {
                # Fallback if not an object
                echo "output:"
                format_json_value "$TOOL_OUTPUT"
            }
        fi
        
    } >> "$LOG_FILE"
fi

# Always exit successfully to not block tool execution
exit 0
