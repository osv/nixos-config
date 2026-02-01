{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.ai;
  claude-but-logo = ./claude.png;
  chii-logo = ./chii.png;
  zai-logo = ./zai-claude.png;
  chii-style = ./chii-style.md;

  # Reusable function to display logo based on terminal type
  displayLogo = logoPath: ''
    LOGO_IMAGE="${logoPath}"

    # Display image based on terminal type
    if [[ -f "$LOGO_IMAGE" ]]; then
      if [[ "$TERM" == "xterm-kitty" ]]; then
        kitten icat --align left "$LOGO_IMAGE"
      elif [[ "$TERM_PROGRAM" == "WezTerm" ]] || [[ "$TERM_PROGRAM" == "iTerm.app" ]] || [[ "$TERM" == "xterm-konsole" ]] || [[ -n "$KONSOLE_VERSION" ]]; then
        printf '\033]1337;File=inline=1:'
        base64 < "$LOGO_IMAGE"
        printf '\a\n'
      fi
    fi
  '';

  # Reusable environment setup for Claude
  claudeEnvSetup = ''
    # Set environment variables only if not already set
    : ''${BASH_MAX_TIMEOUT_MS:=600000}
    : ''${BASH_DEFAULT_TIMEOUT_MS:=300000}
    : ''${CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC:=1}
    : ''${DISABLE_TELEMETRY:=1}
    : ''${DISABLE_ERROR_REPORTING:=1}
    : ''${DISABLE_BUG_COMMAND:=1}

    # Export the variables
    export BASH_MAX_TIMEOUT_MS
    export BASH_DEFAULT_TIMEOUT_MS
    # export CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC
    # export DISABLE_TELEMETRY
    export DISABLE_ERROR_REPORTING
    export DISABLE_BUG_COMMAND

    # # force ansi
    # export COLORTERM=16
    # export TERM=xterm
  '';

  # Environment setup for z.ai API
  zaiEnvSetup = ''
    # z.ai API configuration
    export ANTHROPIC_DEFAULT_OPUS_MODEL=GLM-4.7
    export ANTHROPIC_DEFAULT_SONNET_MODEL=GLM-4.7
    export ANTHROPIC_DEFAULT_HAIKU_MODEL=GLM-4.5-Air
    ANTHROPIC_AUTH_TOKEN="$(pass show z.ai-crush-key)"
    export ANTHROPIC_AUTH_TOKEN
    export ANTHROPIC_BASE_URL="https://api.z.ai/api/anthropic"
    export API_TIMEOUT_MS=3000000
  '';

  my-claude = pkgs.writeShellScriptBin "my-claude" ''
    #!/usr/bin/env bash

    ${displayLogo claude-but-logo}
    ${claudeEnvSetup}

    # Execute claude command with all arguments
    PATH="/run/current-system/sw/bin:$PATH" exec claude "$@"
  '';

  ask-chii = pkgs.writeShellScriptBin "ask-chii" ''
    #!/usr/bin/env bash

    ${displayLogo chii-logo}
    ${claudeEnvSetup}

    # If no arguments - run interactive claude and exit when done
    if [[ $# -eq 0 ]]; then
      PATH="/run/current-system/sw/bin:$PATH" claude
      exit $?
    fi

    # Read Chii style from file
    CHII_STYLE="$(cat ${chii-style})"

    # Combine style with user prompt and pipe through glow for pretty markdown
    PATH="/run/current-system/sw/bin:$PATH" claude -p "$CHII_STYLE

Пользователь спрашивает:
$*" | ${pkgs.glow}/bin/glow
  '';

  my-claude-zai = pkgs.writeShellScriptBin "my-claude-zai" ''
    #!/usr/bin/env bash

    ${displayLogo zai-logo}
    echo $'\033[1;33;41m                      Внимание!                      \033[0m'
    echo $'\033[1;97;41m  Z.AI собирать твой ценный данные славить компартия \033[0m'
    echo $'\033[1;97;41m                                                     \033[0m'
    ${claudeEnvSetup}
    ${zaiEnvSetup}

    # Execute claude command with all arguments
    PATH="/run/current-system/sw/bin:$PATH" exec claude "$@"
  '';
in {
  options.nerv.opt.apps.ai = with types; {
    enable = mkBoolOpt false "Whether or not to enable AI tools.";
    whisper = mkBoolOpt true "Openai Whisper.";
    claude-desktop = mkBoolOpt true "Claude Desktop application.";
    code-cursor = mkBoolOpt true "Code Cursor AI-powered editor.";
    claude = {
      enable = mkBoolOpt true "Claude CLI (claude-code).";
      cleanup-days = mkOpt int 120 "Number of days to keep Claude Code conversation history.";
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.whisper) {
      environment.systemPackages = with pkgs;
        [ (openai-whisper-cpp.override { config = { cudaSupport = true; }; }) ];
    })
    (mkIf (cfg.enable && cfg.claude-desktop) {
      environment.systemPackages = with pkgs; [ claude-desktop ];
      nerv.opt.persist.state.homeDirectories = [ ".config/Claude" ];
    })
    (mkIf (cfg.enable && cfg.code-cursor) {
      environment.systemPackages = with pkgs; [ code-cursor ];
      nerv.opt.persist.state.homeDirectories = [ ".cursor" ".config/Cursor" ];
    })
    (mkIf (cfg.enable && cfg.claude.enable) {
      environment.systemPackages = [ my-claude ask-chii my-claude-zai ];

      nerv.opt.persist.state.homeFiles = [ ".claude.json" ];
      nerv.opt.persist.state.homeDirectories = [
        ".config/claude"
        ".config/direnv" # Add persistence for direnv config
        ".claude"
      ];

      # Add zsh configuration for Claude
      nerv.home.extraOptions.programs.zsh.envExtra = ''
        # Claude Code direnv integration
        # Also need mkdir -p ~/.config/direnv
        # touch ~/.config/direnv/direnv.toml
        # because there is a bug that causes DIRENV_LOG_FORMAT to be ignored if the config
        # file does not exist

        if command -v direnv >/dev/null; then
          if [[ ! -z "$CLAUDECODE" ]]; then
            eval "$(direnv hook zsh)"
            eval "$(DIRENV_LOG_FORMAT= direnv export zsh)"  # Need to trigger "hook" manually
          fi
        fi
      '';

      # Combined Claude configuration activation script
      nerv.home.activation.claudeSetup = ''
        # Setup direnv config directory and file (for Claude Code direnv integration)
        mkdir -p $HOME/.config/direnv
        if [[ ! -e $HOME/.config/direnv/direnv.toml ]]; then
          touch $HOME/.config/direnv/direnv.toml
        fi

        # Configure Claude settings if settings.json exists
        if [[ -f $HOME/.claude/settings.json ]]; then
          # Flag to track if settings were modified
          SETTINGS_MODIFIED=false

          # Check and add statusLine configuration if not present
          if ! ${pkgs.jq}/bin/jq -e '.statusLine' $HOME/.claude/settings.json >/dev/null 2>&1; then
            ${homeEcho "Claude" "Setting up statusline..."}

            # Copy the statusline script
            mkdir -p $HOME/.claude
            cp ${./statusline-script.sh} $HOME/.claude/statusline-script.sh
            chmod +x $HOME/.claude/statusline-script.sh

            # Update settings.json to add statusLine configuration
            ${pkgs.jq}/bin/jq '. + {"statusLine": {"type": "command", "command": "bash ~/.claude/statusline-script.sh"}}' \
              $HOME/.claude/settings.json > $HOME/.claude/settings.json.tmp

            if [[ $? -eq 0 ]]; then
              mv $HOME/.claude/settings.json.tmp $HOME/.claude/settings.json
              ${homeEcho "Claude" "Statusline configured"}
              SETTINGS_MODIFIED=true
            else
              rm -f $HOME/.claude/settings.json.tmp
              ${homeEcho "Claude" "Failed to configure statusline"}
            fi
          fi

          # Check and add cleanupPeriodDays if not present
          if ! ${pkgs.jq}/bin/jq -e '.cleanupPeriodDays' $HOME/.claude/settings.json >/dev/null 2>&1; then
            ${homeEcho "Claude" "Setting cleanup period to ${toString cfg.claude.cleanup-days} days..."}

            # Update settings.json to add cleanupPeriodDays
            ${pkgs.jq}/bin/jq '. + {"cleanupPeriodDays": ${toString cfg.claude.cleanup-days}}' \
              $HOME/.claude/settings.json > $HOME/.claude/settings.json.tmp

            if [[ $? -eq 0 ]]; then
              mv $HOME/.claude/settings.json.tmp $HOME/.claude/settings.json
              ${homeEcho "Claude" "Cleanup period configured"}
              SETTINGS_MODIFIED=true
            else
              rm -f $HOME/.claude/settings.json.tmp
              ${homeEcho "Claude" "Failed to configure cleanup period"}
            fi
          fi

          if [[ "$SETTINGS_MODIFIED" == "true" ]]; then
            ${homeEcho "Claude" "Settings updated successfully"}
          fi
        fi
      '';
    })
  ];
}
