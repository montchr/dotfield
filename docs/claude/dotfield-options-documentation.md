# Complete Dotfield Options Documentation

This document describes all available configuration options under `options.dotfield` in the dotfield flake-parts module system.

## Overview

The dotfield module system provides:
- User metadata and preferences (`dotfield.meta.users`)
- Display metadata (`dotfield.meta.displays`)
- NixOS module definitions (`dotfield.nixos`)
- Home-manager module definitions (`dotfield.home`)
- Modular component system (`dotfield.features`)
- Host configurations (`dotfield.hosts.nixos`)

## Core Options

### `dotfield.options`
- **Type**: `lazyAttrsOf raw`
- **Default**: `{}`
- **Description**: Generic options store

### `dotfield.nixos`
- **Type**: Module type
- **Description**: Global NixOS configuration

### `dotfield.home`
- **Type**: Module type
- **Description**: Global Home-Manager configuration

### `dotfield.features`
- **Type**: `lazyAttrsOf (submodule)`
- **Description**: Modular component definitions with:
  - `nixos`: NixOS module
  - `home`: Home-Manager module

### `dotfield.hosts.nixos`
- **Type**: `attrsOf (submodule)`
- **Description**: Per-host NixOS configurations with:
  - `name`: Host name (read-only)
  - `modules`: List of modules to apply
  - `users`: Per-user configurations
  - `nixos`: Host-specific NixOS config
  - `home`: Host-specific home-manager config

## User Preferences

### `dotfield.meta.users.<name>.preferences`

All options have the `with types;` prefix for their type declarations.

#### Application Preferences
- **`editor`**: `str` (default: `"emacsclient"`) - Default text editor command
- **`term`**: `str` (default: `"ghostty"`) - Default terminal emulator
- **`shell`**: `str` (default: `"fish"`) - Default shell
- **`file-manager`**: `str` (default: `"nemo"`) - Default file manager
- **`audio-player`**: `str` (default: `"mpv"`) - Default audio player
- **`video-player`**: `str` (default: `"mpv"`) - Default video player
- **`web-browser`**: `str` (default: `"firefox"`) - Default web browser

#### Theme Configuration

**`theme.color`**:
- **`variant`**: `enum ["light" "dark"]` (default: `"light"`) - Color theme variant
- **`scheme.dark`**: `str` (default: `"catppuccin-mocha"`) - Dark color scheme name
- **`scheme.light`**: `str` (default: `"catppuccin-latte"`) - Light color scheme name

**`theme.font.families`** (all are submodules with `name` and `pname` string options):
- **`sansSerif`**: Default: `{name = "Inter"; pname = "inter";}`
- **`serif`**: Default: `{name = "Aporetic Serif"; pname = "aporetic";}`
- **`monospace`**: Default: `{name = "Aporetic Sans Mono"; pname = "aporetic";}`

**`theme.font.sizes`** (all `int` type):
- **`applications`**: `12` - Font size for applications
- **`desktop`**: `10` - Font size for desktop elements
- **`popups`**: `10` - Font size for popups
- **`terminal`**: `10` - Font size for terminal

**`theme.icons`** (submodule):
- **`pname`**: `str` (default: `"papirus-icon-theme"`) - Icon theme package name
- **`dark`**: `str` (default: `"Papirus Dark"`) - Dark variant icon theme name
- **`light`**: `str` (default: `"Papirus Light"`) - Light variant icon theme name

**`theme.wallpaper`** (submodule):
- **`image`**: `nullOr str` (default: `null`) - Path to wallpaper image
- **`mode`**: `str` (default: `"fit"`) - Wallpaper display mode

**`theme.cursor`** (submodule):
- **`name`**: `str` (default: `"Bibata-Modern-Classic"`) - Cursor theme name
- **`pname`**: `str` (default: `"bibata-cursors"`) - Cursor theme package name
- **`size`**: `int` (default: `16`) - Cursor size

**`theme.gui`** (submodule):
- **`pname`**: `str` (default: `"flat-remix-gtk"`) - GTK theme package name
- **`name`**: `str` (default: `"Flat-Remix-GTK-Grey-Light"`) - GTK theme name

#### Wayland Configuration

**`wayland`** (submodule, all `str` type):
- **`desktop`**: `"hyprland"` - Wayland desktop environment/compositor
- **`bar`**: `"waybar"` - Wayland status bar
- **`menu`**: `"fuzzel"` - Wayland menu/dmenu replacement
- **`launcher`**: `"fuzzel"` - Wayland application launcher
- **`notifications`**: `"mako"` - Wayland notification daemon

## Display Configuration

### `dotfield.meta.displays.<name>`

**Note**: These options are used but not formally defined in the module system. They appear to be freeform configuration.

- **`dpi`**: `int` - DPI setting for the display

**Available displays**:
- **`LG-27UD88-W`**: dpi = 163 (4K 27" monitor)
- **`LG-27GL850-B`**: dpi = 109 (1440p 27" monitor)
- **`ViewSonic-PX727-4K`**: dpi = 44 (4K projector)

These display configurations are used by hardware modules to set appropriate DPI and scaling settings for X11 and Wayland.

## Usage Examples

### Setting User Preferences

```nix
dotfield.meta.users.myuser.preferences = {
  editor = "nvim";
  term = "kitty";
  theme.color.variant = "dark";
  theme.font.sizes.terminal = 12;
  wayland.desktop = "sway";
};
```

### Using Display Configuration

```nix
# In a host configuration
dotfield.meta.displays."My-Monitor".dpi = 144;

# Referenced in modules
services.xserver.dpi = self.dotfield.meta.displays."My-Monitor".dpi;
```

## Architecture Notes

- Uses flake-parts for modular flake structure
- Separates NixOS system config from home-manager user config
- Host-specific configurations in dotfield.hosts.nixos
- Type-safe configuration with comprehensive defaults
- Modular design allows for composable configurations across multiple hosts
