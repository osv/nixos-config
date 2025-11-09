# Claude Code Tool Usage Logger

This hook logs all Claude Code tool usage (including MCP tools) to markdown files organized by project and session.

## Log Structure

```
/tmp/claude-code-log/
└── <project-name>/
    ├── <session-id-1>.md
    └── <session-id-2>.md
```

## Log Format

The logs are in markdown format with timestamps and structured data:

```markdown
# [2025-01-27 10:30:45] PRE-TOOL: Read
Request:
file_path:
```
/path/to/file.txt
```
limit:
```
100
```

# [2025-01-27 10:30:46] POST-TOOL: Read
Response (exit code: 0):
output:
```
File contents here...
```

---
```

## Installation

1. Make the script executable:
   ```bash
   chmod +x log-tool-usage.sh
   ```

2. Add the hook configuration to your Claude Code settings:
   - Copy the contents of `claude-settings-example.json`
   - Merge it into your `~/.config/claude/settings.json`
   - Update the script paths to match your installation location

3. The hook will automatically create log directories as needed

## Features

- Logs both tool requests (PreToolUse) and responses (PostToolUse)
- Organizes logs by project name and session ID
- Formats complex JSON objects and arrays properly
- Handles both built-in tools and MCP tools
- Non-blocking: won't interfere with Claude Code operation
- Timestamps for each tool invocation

## Viewing Logs

To view logs for the current project and session:
```bash
# Find current session logs
ls -la /tmp/claude-code-log/$(basename "$PWD")/

# View a specific session log
less /tmp/claude-code-log/$(basename "$PWD")/<session-id>.md
```