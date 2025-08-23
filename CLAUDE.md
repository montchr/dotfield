# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About Dotfield

Dotfield is a personal NixOS/home-manager configuration (dotfiles) repository using flakes and flake-parts architecture. The project manages declarative system and user configurations across multiple machines including NixOS systems and macOS hosts.

## Common Development Commands

### Core Build/Switch Commands
- `just build` - Rebuild the system and provide a summary of changes (uses nh + cachix)
- `just switch` - Rebuild and switch to the new system generation
- `just boot` - Build system for activation on next boot
- `just home <args>` - Build/switch home-manager configurations

### Development Tools
- `just check` - Run flake checks
- `just lint` - Lint Nix files using statix and deadnix
- `just fix` - Apply linter fixes automatically
- `just fmt [FILES]` - Format files using treefmt

### Testing and Quality
- `nix flake check` - Validate flake outputs
- `statix check` - Check for Nix code issues
- `deadnix --fail` - Check for unused code
- `treefmt --no-cache` - Format all files

### Theme Management
- `just theme <kind>` - Set theme across all applications ('light' or 'dark')
- `just light` - Switch to light theme
- `just dark` - Switch to dark theme

## Project Architecture

### Directory Structure
- `/flake.nix` - Main flake configuration with inputs and outputs
- `/machines/` - Host-specific configurations (boschic, ryosuke, tuuvok, etc.)
- `/home/` - Home-manager configurations and user-specific settings
  - `/home/profiles/` - Modular home configurations (shells, graphical, development, etc.)
  - `/home/mixins/` - Composable feature sets
- `/nixos/` - NixOS system-level configurations
  - `/nixos/profiles/` - System profiles (graphical, audio, hardware, etc.)
  - `/nixos/mixins/` - System-level feature mixins
- `/users/` - User-specific configurations per host
- `/ops/` - Operations data (keys, hosts, networks)
- `/packages/` - Custom package definitions
- `/src/lib/` - Custom Nix library functions

### Key Concepts
- Uses flake-parts for modular flake structure
- Separates NixOS system config from home-manager user config
- Host-specific configurations in `/machines/<hostname>/`
- Profiles and mixins pattern for composable configurations
- SOPS-nix for secrets management
- Multiple nixpkgs channels (stable, unstable, trunk)

### User Management
Users are defined per-host in `/users/<username>/<username>-at-<hostname>.nix` files. The main user is `cdom` with configurations across multiple hosts.

### Secrets Management
Uses SOPS-nix with age encryption. Keys stored in `/ops/data/keys/`. Secrets files are `.yaml.age` encrypted and must be decrypted for editing.

## Important Files
- `/.justfile` - Main task runner with common development commands
- `/hive.nix` - Colmena deployment configuration for remote hosts
- `/treefmt.toml` - Code formatting configuration
- `/statix.toml` - Nix linting configuration

## Build System Notes
- Uses cachix binary cache ("dotfield" cache)
- Builds can be expensive; uses nh (Nix Helper) wrapper
- Remote builders configured for distributed building
- Some hosts (tuuvok) are aarch64-linux (Apple Silicon via Asahi Linux)

## Development Workflow
1. Make changes to relevant configuration files
2. Run `just check` to validate flake
3. Run `just build` or `just switch` to test changes
4. Use `just lint` and `just fmt` before committing
5. Test across multiple hosts if making system-wide changes

## Known Issues and Maintenance Challenges

### Configuration Inconsistencies
- **User configuration patterns vary across hosts**: Some use `users/` directories, others inline user definitions
- **Mixed import patterns**: Some hosts use relative imports (`../../../users/cdom/`), others use different path structures  
- **Inconsistent home-manager integration**: Mix of `import` statements vs inline definitions
- **Scattered secrets management**: SOPS configuration sometimes in separate files, sometimes inline

### Code Duplication
- **Repeated user boilerplate**: User definitions with identical patterns (uid=1000, wheel group, SSH keys) duplicated across hosts
- **Common system settings**: `time.timeZone = "America/New_York"` hardcoded in 4+ hosts
- **Boot loader settings**: `boot.loader.efi.canTouchEfiVariables = true` repeated across hosts
- **Network services**: `services.tailscale.enable = true` duplicated instead of using a common mixin

### Hard-coded Values That Should Be Abstracted
- Timezone (`America/New_York`) should be a global default or per-user setting
- SSH authorized keys pattern (`ops.users.cdom.keys.default`) repeated everywhere
- System state versions vary without clear rationale (21.11, 22.05, 23.05, 23.11, 24.05)
- UID assignments (multiple users with uid=1000)

### Structural Issues
- **Mixed organization patterns**: Some hosts organize by function (users/, secrets/), others mix everything in default.nix
- **Inconsistent file naming**: Some use `default.nix`, others use descriptive names
- **Home-manager path inconsistencies**: Mix of `../../../users/cdom/cdom-at-<host>.nix` vs `../../users/cdom/cdom-at-<host>.nix`

## Refactoring Recommendations

### 1. User Configuration Abstraction
Create a common user module in `/nixos/modules/` that handles:
- Standard user creation with configurable UIDs
- Automatic SSH key assignment from ops data  
- Standard group assignments (wheel, etc.)
- Consistent home-manager integration

### 2. Common System Defaults Module
Extract repeated system settings into a shared module:
- Default timezone (with per-host override capability)
- Common boot loader settings
- Standard network service enablement (Tailscale)
- Consistent state version management strategy

### 3. Standardize Host Structure
Establish consistent patterns for host organization:
- Standardize on either `users/` directories OR inline definitions, not both
- Consistent import path patterns (consider using absolute paths from flake root)
- Standard secrets organization (separate sops.nix files vs inline)

### 4. Create Host Categories/Mixins
Abstract common host patterns:
- Desktop/workstation hosts (boschic, hodgepodge, ryosuke, tuuvok)
- Server hosts (hierophant, chert, gabbro)  
- Special-purpose hosts (build machines, installers)

### 5. Secrets Standardization
- Consistent SOPS file organization across hosts
- Standard secret naming conventions
- Common patterns for secrets that need to be available to multiple hosts

## Special Considerations
- Apple Silicon support via nixos-apple-silicon flake
- Multi-architecture builds (x86_64-linux, aarch64-linux)
- Complex audio setup on some hosts (Focusrite Scarlett interfaces)
- Wayland-first with fallback X11 support
- Extensive theming system with light/dark variants
- `rg` and `fd` are available tools
- ask permission when executing commands via `fd`