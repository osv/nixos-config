# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a NixOS flake configuration repository using the Snowfall library framework. The primary host system is "xenon" with additional hosts "argon", "jasper", and "krypton". The configuration uses ZFS, impermanence (optionally on tmpfs root), and comprehensive modular organization. Update this file on adding or removing hosts.

## Key Commands

### Building and Switching Configuration
```bash
# Standard rebuild with verbose output and monitoring
sudo nixos-rebuild switch -v --flake . |& nom

# Using the configured alias
nrs

# Using nh (Yet another nix cli helper) - installed by default
nh os switch .
```

### Comparing Generations
```bash
# Show diff between current and previous generation
my-nixos-diff
```

### Formatting Nix Code
```bash
# Format nix files
nixfmt-classic <file.nix>
```

## Architecture Overview

### Flake Structure
- **flake.nix**: Main entry point using Snowfall lib with namespace "nerv"
- **Systems**: Host configurations in `systems/x86_64-linux/`
- **Modules**: Reusable NixOS modules in `modules/nixos/`
- **Overlays**: Package overlays in `overlays/`
- **Packages**: Custom packages in `packages/`

### Module Organization
Modules are organized by function in `modules/nixos/`:
- `opt/`: All optional modules (most modules)
  - `apps/`: GUI applications (emacs, firefox, discord, etc.)
  - `cli-apps/`: CLI tools (tmux, wine, etc.)
  - `desktop/`: Desktop environments and utilities (xmonad, sway, gnome)
  - `gaming/`: Gaming-related configurations
  - `hardware/`: Hardware configurations (audio, networking, fingerprint)
  - `nix/`: Nix-specific configurations
  - `persist/`: Persistence configurations for impermanence
  - `programming/`: Programming language environments (node, python, ruby)
  - `security/`: Security tools (gpg, pass, sops, vpn)
  - `services/`: System services (openssh, tailscale, etc.)
  - `system/`: Core system config (boot, fonts, zfs, performance)
  - `tools/`: Development tools (git, docker, k8s, etc.)
  - `user/`: User-specific configurations
  - `virtualisation/`: Virtualisation tools (docker, kvm)
- `archetypes/`: System profiles (workstation, gaming, server, minimal)
- `suites/`: Application bundles (development, media, games, etc.)
- `home/`: Home-manager integration

**Used the following conventions**:

* Modules are stored in the `modules/nixos/` directory. The `default.nix` file for each module is imported automatically, so manual import is not required.
* Additional Nix files may be imported within a module for better organization, if needed.
* The `options.nerv` namespace is used for module configurations in this repository.
* Each NixOS module exports `options` and `config`.
* The naming convention for NixOS options uses the path to the `default.nix` file as the option path.
  For example, the module at `modules/nixos/archetypes/workstation/default.nix` exports `options.nerv.archetypes.workstation` options.
* Modules may have nested options. For example, for the option `nerv.opt.system.zfs.auto-snapshot.enable`, the module file is `nixos/opt/system/zfs/default.nix`, as there is no `nixos/opt/system/zfs/auto-snapshot/default.nix` file.
* Always add an `enable` option to each new module, which is `false` by default, but new nested options of a module should be enabled for new modules.
* Nested module options are allowed, but if there are many apps, consider creating a subdirectory.
* Modules must have persistent state configuration for any apps they manage (i.e., configs, artifact directories, or files used by apps).
* All modules must be enabled (or disabled) in a "suite" module (e.g., `nerv.suites.development`).
* All suites must be enabled (or disabled) in an "archetypes" module (e.g., `nerv.archetypes.workstation`).
* A host-specific Nix file must enable an "archetypes" module. Some modules can be enabled or disabled directly in the host, but new modules should always be added to a suite.

Read `./MODULES.md` to see what modules are available.
Read `lib/module/default.nix` for util functions for module. For suites or archetypes: do not use `foo.enable = true`, but `foo = yes` or `foo = enabled`; use `yes`/`no` instead of `true`/`false` for suites or archetypes.

### Key Technologies
- **ZFS**: Primary filesystem with automatic snapshots
- **Impermanence**: Optional tmpfs root with persistent state management
- **SOPS**: Secret management for sensitive data
- **Disko**: Declarative disk partitioning
- **Home-manager**: User environment management
- **Deploy-rs**: Remote deployment capability
- **Emacs**: Main editor, used Doom config files in directory `modules/nixos/apps/emacs/config_doom/`. Used `lib.makeBinPath emacsEnvPackages` for setup all needed apps for Emacs (LSP servers, etc) and controlled by separate options in `modules/nixos/apps/emacs/default.nix`. 
- **XMonad**: Location of config - `modules/nixos/desktop/xmonad/.xmonad/xmonad.hs`

### Overlays
Package overlays are managed in `overlays/` directory:

- **Unstable overlay**: `overlays/unstable/default.nix` - centralized location for all packages that use `channels.unstable`. When adding new packages from unstable, add them here instead of creating separate overlay files.
- **Complex overlays**: Custom overlays with specific overrides or complex logic remain in individual directories (e.g., `clickhouse/`, `gnome/`, `openai-whisper-cpp/`)

### Packaging
Packages are managed using Snowfall lib's automatic discovery system:

- **Location**: `packages/<name>/default.nix` - each package gets its own directory
- **Template**: Standard parameters available to all packages:
  ```nix
  {
    lib,           # Customized library instance
    inputs,        # Flake inputs
    namespace,     # Flake namespace ("nerv")
    pkgs,          # NixPkgs packages
    stdenv,        # Standard build environment
    ...
  }:
  ```
- **Discovery**: Packages automatically exported to `outputs.packages.<system>.<name>`
- **Integration**: Available to modules, overlays, and other systems across the flake
- **Git requirement**: Must run `git add` when creating new package files

**Examples in repository**:
- `my-scripts`: Simple shell script using `pkgs.writeShellScriptBin`
- `wallpapers`: Asset package with `builtins.readDir` discovery
- `wcwidth-icons`: Compiled C library with custom build/install phases
- `steam`: Desktop entries package using `makeDesktopItem` and `symlinkJoin`

### Development Shells
Development shells provide isolated environments for different programming languages and projects:

- **Location**: `shells/<name>/default.nix` - each shell gets its own directory
- **Template**: Standard parameters similar to packages:
  ```nix
  {
    lib,           # Customized library instance
    inputs,        # Flake inputs
    namespace,     # Flake namespace ("nerv")
    pkgs,          # NixPkgs packages
    ...
  }:
  pkgs.mkShell {
    packages = with pkgs; [
      # Shell dependencies
    ];
  }
  ```
- **Discovery**: Shells automatically exported to `outputs.devShells.<system>.<name>`
- **Usage**: Enter shells with `nix develop .#<name>` or use with direnv
- **Git requirement**: Must run `git add` when creating new shell files

### Host-Specific Notes
- **xenon**: Main workstation with NVIDIA GPU, gaming support, ZFS on NVMe
- **krypton**: Dell Latitude 5530 laptop, portable workstation configuration
- Uses custom persistence paths under `/persist` for stateful data
- Configured with workstation and gaming archetypes

## Important Configuration Patterns

### Module Enabling Pattern
```nix
nerv = {
  apps.firefox = on;  # Simple enable
  system.zfs = on // { auto-snapshot = off; };  # Enable with options
};
```

### Simplified Module Creation with `stdModules`

For simple modules that only need package installation and persistence configuration, use the `stdModules` helper function to reduce boilerplate.

**Location**: Available as `lib.nerv.stdModules` in `lib/module/default.nix`

**When to use**:
- Simple modules with just packages and/or persistence
- Multiple similar modules that can be grouped in one file
- Modules without complex custom options or special logic

**When NOT to use**:
- Modules with custom options beyond `enable`
- Modules requiring home-manager integration
- Modules with complex conditional logic or systemd services
- Modules that need special configuration structures

**Syntax**:
```nix
{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    path              # Module option path (string)
    description       # Short name for enable option description (string)
    packages          # List of packages to install (list)
    persistConfig     # Persistence configuration (attrset)
    extraConfig       # Additional NixOS config (attrset)
  ]
  # ... more modules
]
```

**Parameters**:
- `path`: Module option path, e.g., `"nerv.opt.cli-apps.wine"`
- `description`: Short name used in enable option description, e.g., "VLC (Video player)" generates "Whether or not to enable Wine.". 
  **CRITICAL**: Always describe what this app or module is used for. For example: "dolphin (KDE file manager)" or "xkb (keyboard layout configuration)"".
- `packages`: List of packages to install in `environment.systemPackages`, e.g., `(with pkgs; [package1 package2])`
- `persistConfig`: Persistence configuration matching `nerv.opt.persist` structure, e.g., `{ derivative.homeDirectories = [ ".wine" ]; }`
- `extraConfig`: Any additional NixOS configuration, e.g., `{ nerv.opt.user.extraGroups = [ "video" ]; }`

**Examples**:

Simple module with packages only:
```nix
stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.apps.vlc"
    "VLC (Video player)"
    (with pkgs; [ vlc ])
    {}
    {}
  ]
]
```

Module with packages and persistence:
```nix
stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.cli-apps.wine"
    "Wine"
    (with pkgs; [
      winePackages.unstable
      winetricks
      wine64Packages.unstable
    ])
    { derivative.homeDirectories = [ ".local/share/wineprefixes/default" ]; }
    {}
  ]
]
```

Module with additional configuration:
```nix
stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.tools.docker"
    "Docker"
    (with pkgs; [ docker docker-compose ])
    { state.directories = [ "/var/lib/docker" ]; }
    {
      nerv.opt.user.extraGroups = [ "docker" ];
      virtualisation.docker.enable = true;
    }
  ]
]
```

Multiple modules in one file:
```nix
stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.cli-apps.wine"
    "Wine"
    (with pkgs; [ winePackages.unstable winetricks ])
    { derivative.homeDirectories = [ ".wine" ]; }
    {}
  ]
  [
    "nerv.opt.cli-apps.wireshark"
    "Wireshark"
    (with pkgs; [ wireshark ])
    { state.homeDirectories = [ ".config/wireshark" ]; }
    { nerv.opt.user.extraGroups = [ "wireshark" ]; }
  ]
]
```

**File location pattern**:
When using `stdModules`, create or update `modules/nixos/opt/<category>/default.nix` to contain all simple modules for that category. For example:
- `modules/nixos/opt/cli-apps/default.nix` - Contains all simple CLI app modules
- `modules/nixos/opt/apps/default.nix` - Contains all simple GUI app modules

Complex modules should still have their own subdirectory with a `default.nix` file.

### Enabling Modules in Suites

After creating a module, you must enable it in an appropriate suite. Modules are NOT automatically enabled.

**Suite organization** (`modules/nixos/suites/`):
- `common` / `common-slim`: Base system utilities
- `desktop`: Core desktop apps (browsers, file manager, media viewers)
- `development`: Development tools (editors, databases, docker)
- `art`: Creative tools (GIMP, Inkscape, Blender)
- `video`: Video editing (Shotcut, SimpleScreenRecorder)
- `music`: Audio production (Ardour)
- `media`: Media streaming (FreeTube)
- `social`: Communication (Telegram, Discord, Zoom)
- `games`: Gaming (Steam, Lutris)
- `emulation`: Game emulation (Yuzu, PCSX2)

**How to enable a module in a suite**:

1. Identify the appropriate suite based on the module's function
2. Open the suite file: `modules/nixos/suites/<suite-name>/default.nix`
3. Add the module to the `opt.apps` (or appropriate category) section using `on`

**Example** - enabling qimgv in the desktop suite:
```nix
# In modules/nixos/suites/desktop/default.nix
nerv = {
  opt.apps = {
    firefox = on;
    vlc = on;
    qimgv = on;    # Add new module here
    zathura = on;
  };
};
```

**Suite selection guidelines**:
- **desktop**: Lightweight essential utilities (viewers, file managers, browsers)
- **development**: IDEs, editors, programming tools
- **art/video/music**: Specialized creative/production tools
- **games/emulation**: Gaming-related applications

**Complete workflow for adding a new app**:
1. Add module definition to `modules/nixos/opt/apps/default.nix` using `stdModules`
2. Enable the module in the appropriate suite
3. Run `git add` on modified files
4. Update `MODULES.md` if adding a new module
5. Test build: `nixos-rebuild build --flake .#<hostname>`

### Persistence Configuration
When `isRootOnTmpFS = true`, stateful data is preserved in:
- `/persist/state/`: System state
- `/persist/cache/`: Caches
- `/persist/derivative`: Cached files that should not be backup ed, for example artifacts created by package managers.

Separate options for state, cache, derivative available:
`files`, `directories`, `homeDirectories`, `homeFiles`,
where "home" means user's files `~/`,
e.g. `nerv.opt.persist.state.homeDirectories = [ '.config/github-copilot' ]`

Store persistence cfg of packages that are package manager in as derivative in `modules/nixos/persist/common`. Add options that must be true by default.

### Boot-time Initialization for tmpfs Root

On systems with `isRootOnTmpFS = true`, files in home directory are lost on reboot. For cases where you need to create default files at boot (not persist them), use one of these approaches:

**When to use**:
- Setting default application state that user can modify during session
- Creating initial config files that will be regenerated on next boot
- Host-specific initialization that shouldn't be in persistence

**Approach 1: System Activation Script** (recommended for pre-login init)
Runs at every boot, before user login. Use for files that must exist before user services start.

```nix
# In host config (systems/x86_64-linux/<host>/default.nix)
# Get username from user module (don't hardcode!)
let userName = config.nerv.opt.user.name;
in {
  system.activationScripts.<name>.text = ''
    mkdir -p /home/${userName}/<path>
    cat > /home/${userName}/<path>/<file> << 'EOF'
    <content>
    EOF
    chown -R ${userName}:users /home/${userName}/<path>
  '';
}
```

**Approach 2: systemd User Service** (for post-login init)
Runs after user login. Use when you need user environment or when timing with other user services matters.

```nix
nerv.home.extraOptions = {
  systemd.user.services.<name> = {
    Unit = {
      Description = "<description>";
      Before = [ "<service-to-run-before>.service" ];  # optional
    };
    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "<name>" ''
        mkdir -p "$HOME/<path>"
        cat > "$HOME/<path>/<file>" << 'EOF'
        <content>
        EOF
      '';
    };
    Install.RequiredBy = [ "<service>.service" ];  # or WantedBy = [ "default.target" ];
  };
};
```

**Comparison**:
| Approach | Runs | Use case |
|----------|------|----------|
| `system.activationScripts` | Every boot, before login | Files needed by system or before user services |
| `systemd.user.services` | After user login | Files needed by specific user services |

**Note**: For files that should persist across reboots, use `nerv.opt.persist.*` instead.

### Activation script

**Colored output helpers** (in `lib/module/default.nix`):
- `sysEcho tag msg` — for `system.activationScripts` (magenta)
- `homeEcho tag msg` — for `nerv.home.activation` (blue)

Usage:
```nix
system.activationScripts.foo.text = ''
  ${lib.nerv.sysEcho "MyTag" "Doing something..."}
'';

nerv.home.activation.bar = ''
  ${homeEcho "MyTag" "Doing something..."}
'';
```

## Development Workflow

1. Make changes to relevant module files.
   **CRITICAL**: Update `./MODULES.md` on adding new modules or module rename.
   **CRITICAL**: Always describe what this module is used for. For example: "dolphin (KDE file manager)" or "xkb (keyboard layout configuration)"".
   **CRITICAL:** Always update `modules/nixos/opt/user/README.zsh.md` after adding new packages to Zsh or changing keybindings. Keep this file up to date.
2. **CRITICAL**: you must `git add` created files. Files must be in git, otherwise nix flake cannot build.
2. Test build: `nixos-rebuild build --flake .#xenon`
3. Apply changes only if user request:
   - switch ``nixos-rebuild switch --flake .#xenon`
   - Check generation diff: `my-nixos-diff`

where `xenon` - is hostname example. Current hostname: !`hostname`

### Commit Message Convention

Follow the established commit message style in this repository:

**Format**:
```
<scope>: <description>
```
or simply:
```
<description>
```

**Rules**:
1. **Language**: English only
2. **Scope** (optional): Indicates the area of changes
   - Examples: `zsh`, `emacs`, `firefox`, `CLAUDE.md`, `zsh/emacs`
   - Use `/` to combine related scopes
3. **Description**: Brief description of changes
   - Start with a capital letter
   - Use imperative mood (command form)
   - Common verbs: `Add`, `Fix`, `Update`, `Enable`, `Disable`, `Remove`, `Move`, `Replace`, `Swap`, `Switch`, `Refactor`
4. **Multiple changes**: Combine with `and`

**Examples from this repository**:
- `zsh/emacs: Change file completion keybinding to Ctrl+X /`
- `firefox: Enable Firefox and migrate search engine favicons to local storage`
- `Add yazi file manager and update Chii style emoji options`
- `Fix Krypton disco`

## Notes

- Tests are part of NixOS module system checks and assertions
- The repository uses Snowfall lib conventions for automatic module discovery
- Git commits should follow existing patterns in the repository
- Ensure to use "$$" in nix files when you need to put `$` char in multi line variable text (double quoted by `''` in nix) to prevent interpolation 
