#!/usr/bin/env bash
# Generate main index page for keybindings documentation
# Scans ~/.cache/nixos-config/keybinding/ for subdirectories with index.html
# and generates a main index.html with links to all found apps

set -euo pipefail

KEYBINDING_DIR="$HOME/.cache/nixos-config/keybinding"
INDEX_FILE="$KEYBINDING_DIR/index.html"

# Ensure base directory exists
mkdir -p "$KEYBINDING_DIR"

# Find all subdirectories with index.html
apps=""
for dir in "$KEYBINDING_DIR"/*/; do
  if [ -d "$dir" ] && [ -f "$dir/index.html" ]; then
    app_name=$(basename "$dir")
    # Get modification time
    mod_time=$(stat -c "%Y" "$dir/index.html" 2>/dev/null || echo "0")
    mod_date=$(date -d "@$mod_time" "+%Y-%m-%d %H:%M" 2>/dev/null || echo "Unknown")
    apps="$apps$app_name|$mod_date
"
  fi
done

# Generate app cards HTML
app_cards=""
while IFS='|' read -r app_name mod_date; do
  [ -z "$app_name" ] && continue

  # Capitalize app name for display
  display_name=$(echo "$app_name" | sed 's/.*/\u&/')

  app_cards="$app_cards
        <a href=\"$app_name/index.html\" class=\"app-card\">
          <div class=\"app-icon\">
            <span class=\"icon-letter\">${display_name:0:1}</span>
          </div>
          <div class=\"app-info\">
            <h2>$display_name</h2>
            <p class=\"description\">Keyboard shortcuts documentation</p>
            <p class=\"updated\">Updated: $mod_date</p>
          </div>
          <div class=\"arrow\">→</div>
        </a>"
done <<< "$apps"

# Generate HTML with Doom One theme
cat > "$INDEX_FILE" << 'HTMLEOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Keybindings Documentation</title>
  <style>
    :root {
      --bg: #282c34;
      --bg-alt: #21242b;
      --base0: #1B2229;
      --base1: #1c1f24;
      --base2: #202328;
      --base3: #23272e;
      --base4: #3f444a;
      --base5: #5B6268;
      --base6: #73797e;
      --base7: #9ca0a4;
      --base8: #DFDFDF;
      --fg: #bbc2cf;
      --fg-alt: #5B6268;
      --red: #ff6c6b;
      --orange: #da8548;
      --green: #98be65;
      --teal: #4db5bd;
      --yellow: #ECBE7B;
      --blue: #51afef;
      --dark-blue: #2257A0;
      --magenta: #c678dd;
      --violet: #a9a1e1;
      --cyan: #46D9FF;
    }

    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }

    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
      background: var(--bg);
      color: var(--fg);
      min-height: 100vh;
      padding: 2rem;
    }

    .container {
      max-width: 900px;
      margin: 0 auto;
    }

    header {
      text-align: center;
      margin-bottom: 3rem;
      padding-bottom: 2rem;
      border-bottom: 1px solid var(--base4);
    }

    h1 {
      font-size: 2.5rem;
      color: var(--blue);
      margin-bottom: 0.5rem;
    }

    .subtitle {
      color: var(--base7);
      font-size: 1.1rem;
    }

    .apps-grid {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    .app-card {
      display: flex;
      align-items: center;
      gap: 1.5rem;
      padding: 1.5rem;
      background: var(--bg-alt);
      border: 1px solid var(--base4);
      border-radius: 12px;
      text-decoration: none;
      color: inherit;
      transition: all 0.2s ease;
    }

    .app-card:hover {
      border-color: var(--blue);
      background: var(--base3);
      transform: translateX(4px);
    }

    .app-icon {
      width: 60px;
      height: 60px;
      border-radius: 12px;
      background: linear-gradient(135deg, var(--blue), var(--cyan));
      display: flex;
      align-items: center;
      justify-content: center;
      flex-shrink: 0;
    }

    .icon-letter {
      font-size: 1.8rem;
      font-weight: bold;
      color: var(--bg);
    }

    .app-info {
      flex: 1;
    }

    .app-info h2 {
      font-size: 1.4rem;
      color: var(--fg);
      margin-bottom: 0.3rem;
    }

    .app-info .description {
      color: var(--base7);
      font-size: 0.95rem;
      margin-bottom: 0.3rem;
    }

    .app-info .updated {
      color: var(--base6);
      font-size: 0.85rem;
    }

    .arrow {
      font-size: 1.5rem;
      color: var(--base5);
      transition: all 0.2s ease;
    }

    .app-card:hover .arrow {
      color: var(--blue);
      transform: translateX(4px);
    }

    .empty-state {
      text-align: center;
      padding: 4rem 2rem;
      color: var(--base6);
    }

    .empty-state h2 {
      color: var(--base7);
      margin-bottom: 1rem;
    }

    .empty-state code {
      display: block;
      margin-top: 1rem;
      padding: 0.5rem 1rem;
      background: var(--base2);
      border-radius: 4px;
      font-family: 'JetBrains Mono', 'Fira Code', monospace;
    }

    footer {
      margin-top: 3rem;
      padding-top: 2rem;
      border-top: 1px solid var(--base4);
      text-align: center;
      color: var(--base6);
      font-size: 0.9rem;
    }

    footer a {
      color: var(--blue);
      text-decoration: none;
    }

    footer a:hover {
      text-decoration: underline;
    }

    @media (max-width: 600px) {
      body {
        padding: 1rem;
      }

      h1 {
        font-size: 1.8rem;
      }

      .app-card {
        padding: 1rem;
        gap: 1rem;
      }

      .app-icon {
        width: 50px;
        height: 50px;
      }

      .icon-letter {
        font-size: 1.5rem;
      }
    }
  </style>
</head>
<body>
  <div class="container">
    <header>
      <h1>Keybindings</h1>
      <p class="subtitle">Keyboard shortcuts documentation for my NixOS configuration</p>
    </header>

    <div class="apps-grid">
HTMLEOF

# Insert app cards or empty state
if [ -z "$app_cards" ]; then
  cat >> "$INDEX_FILE" << 'HTMLEOF'
      <div class="empty-state">
        <h2>No keybindings found</h2>
        <p>Keybinding documentation will appear here after you export them from your applications.</p>
        <code>~/.cache/nixos-config/keybinding/&lt;app-name&gt;/index.html</code>
      </div>
HTMLEOF
else
  echo "$app_cards" >> "$INDEX_FILE"
fi

# Close HTML
cat >> "$INDEX_FILE" << 'HTMLEOF'
    </div>

    <footer>
      <p>Generated by <a href="https://github.com/osv/nixos-config/tree/master/packages/my-generate-keybindings-index/">my-generate-keybindings-index</a></p>
    </footer>
  </div>
</body>
</html>
HTMLEOF

echo "Generated: $INDEX_FILE"
