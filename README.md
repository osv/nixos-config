---
title: README
---

<div align="center">
  <img src="modules/nixos/opt/apps/ai/claude.png" alt="Claude" width="128">
  <br>
  <sub>Pwned by Claude Code</sub>
</div>

---

# Features
- disko and ZFS.
- Secrets managed by [SOPS](https://github.com/Mic92/sops-nix).
- Manuals for myself. Convert markdown readme files in this repo into HTML and put into [~/Manuals](~/Manuals) directory.

# Documentation
- [ZSH Configuration](modules/nixos/opt/user/README.zsh.md) - Shell commands, key bindings, and plugins
- [Keybindings](docs/keybinding/) - Keyboard shortcuts for XMonad, Emacs, etc.

## Keybindings Documentation

Keybindings are auto-generated to `~/.cache/nixos-config/keybinding/`:

| Application | When generated | Source |
|-------------|----------------|--------|
| XMonad | On XMonad startup | `KeybindingsExport.hs` |
| Emacs | On `SPC h E` or `M-S-F1` | `keybindings-export.el` |

- **View**: Press `M-S-F1` in XMonad or Emacs to open the main index page
- **Update docs/**: Run `make update-doc` to copy from cache to repository

# Helpful tips
- Switch to new config `nrs` or `nh os switch .` or `nixos-rebuild switch --flake .#xenon`

