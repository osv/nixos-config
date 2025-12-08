{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.ai;
  claude-but-logo = ./claude.png;

  my-claude = pkgs.writeShellScriptBin "my-claude" ''
    #!/usr/bin/env bash

    # Display Claude image based on terminal type
    if [[ "$TERM" == "xterm-kitty" ]]; then
      # Kitty terminal - use kitten icat (most optimized)
      kitten icat --align left ${claude-but-logo}
    elif [[ "$TERM_PROGRAM" == "WezTerm" ]] || [[ "$TERM_PROGRAM" == "iTerm.app" ]] || [[ "$TERM" == "xterm-konsole" ]] || [[ -n "$KONSOLE_VERSION" ]]; then
      # WezTerm, iTerm2, or Konsole - use iTerm2 image protocol
      if [[ -f "${claude-but-logo}" ]]; then
        printf '\033]1337;File=inline=1:'
        base64 < ${claude-but-logo}
        printf '\a\n'
      fi
    fi

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
      environment.systemPackages = [ my-claude ];

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
            echo "Setting up Claude Code statusline..."

            # Copy the statusline script
            mkdir -p $HOME/.claude
            cp ${./statusline-script.sh} $HOME/.claude/statusline-script.sh
            chmod +x $HOME/.claude/statusline-script.sh

            # Update settings.json to add statusLine configuration
            ${pkgs.jq}/bin/jq '. + {"statusLine": {"type": "command", "command": "bash ~/.claude/statusline-script.sh"}}' \
              $HOME/.claude/settings.json > $HOME/.claude/settings.json.tmp

            if [[ $? -eq 0 ]]; then
              mv $HOME/.claude/settings.json.tmp $HOME/.claude/settings.json
              echo "Claude Code statusline configured"
              SETTINGS_MODIFIED=true
            else
              rm -f $HOME/.claude/settings.json.tmp
              echo "Failed to configure Claude Code statusline"
            fi
          fi

          # Check and add cleanupPeriodDays if not present
          if ! ${pkgs.jq}/bin/jq -e '.cleanupPeriodDays' $HOME/.claude/settings.json >/dev/null 2>&1; then
            echo "Setting Claude Code cleanup period to ${toString cfg.claude.cleanup-days} days..."

            # Update settings.json to add cleanupPeriodDays
            ${pkgs.jq}/bin/jq '. + {"cleanupPeriodDays": ${toString cfg.claude.cleanup-days}}' \
              $HOME/.claude/settings.json > $HOME/.claude/settings.json.tmp

            if [[ $? -eq 0 ]]; then
              mv $HOME/.claude/settings.json.tmp $HOME/.claude/settings.json
              echo "Claude Code cleanup period configured"
              SETTINGS_MODIFIED=true
            else
              rm -f $HOME/.claude/settings.json.tmp
              echo "Failed to configure Claude Code cleanup period"
            fi
          fi

          if [[ "$SETTINGS_MODIFIED" == "true" ]]; then
            echo "Claude Code settings updated successfully"
          fi
        fi
      '';
    })
  ];
}
