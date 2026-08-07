# XMonad Module

This module provides XMonad window manager configuration.

## Adding Small Utilities

When adding small utilities needed for XMonad keybindings or functionality:

1. **Add to `default.nix`** in `environment.systemPackages`:
   ```nix
   environment.systemPackages = with pkgs; [
     brightnessctl  # For brightness control keys
     playerctl      # For media control keys
     # ... other packages
   ];
   ```

2. **Why here?** These packages are required for XMonad to function properly.
   They must be available when XMonad starts so keybindings can work immediately.

## Adding Module Options

When enabling or configuring Nerv module options:

1. **Add to `default.nix`** in `config` section:
   ```nix
   nerv = {
     opt.desktop.xmonad = on // {
       # custom options here
     };
   };
   ```

2. **Common patterns:**
   - Enable features: `feature = yes`
   - Configure settings: `setting = "value"`

## Keybindings Structure

Keybindings are defined in `.xmonad/lib/My/Keybindings.hs` using the custom DSL:
- Use `category` to create keybinding groups
- Use `bind` for individual keybindings
- Import `Graphics.X11.ExtraTypes.XF86` for multimedia keys
- XF86 keys format: `"<XF86KeyName>"`
- Modified keys format: `"M-KP_4"`

## Examples

### Adding a new utility for keybindings:
```nix
# In default.nix
environment.systemPackages = with pkgs; [
  some-tool  # Add this
];
```

### Adding XF86 multimedia keys:
```haskell
# In lib/My/Keybindings.hs
import Graphics.X11.ExtraTypes.XF86

-- Add to myKeybindingCategories
, category "My Category" "my-category"
    [ bind "<XF86MyKey>" "Description" $ spawn "my-command"
    ]
```
