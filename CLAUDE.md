# Dotfield - NixOS Configuration

## Overview
Dotfield is a comprehensive NixOS configuration system ("dotfiles") using Nix flakes for managing multiple machines and users. It's built around a modular architecture with features, hosts, and users as the primary organizational units.

## Project Structure

```
/etc/nixos/
├── flake.nix              # Main flake definition with inputs and outputs
├── hive.nix               # Colmena deployment configuration
├── src/                   # Main source code
│   ├── features/          # Modular feature configurations
│   ├── hosts/             # Host-specific configurations
│   ├── lib/               # Custom Nix library functions
│   ├── meta/              # Metadata and constants
│   ├── modules/           # Custom NixOS/Home Manager modules
│   ├── packages/          # Custom package definitions
│   └── users/             # User-specific configurations
├── dev/                   # Development environment tools
├── tests/                 # Test configurations
├── secrets/               # SOPS-encrypted secrets
├── overlays/              # Nix package overlays
└── npins/                 # Pinned dependencies
```

## Key Technologies & Dependencies

- **Nix Flakes**: Modern Nix configuration management
- **flake-parts**: Modular flake structure framework
- **Home Manager**: User environment management
- **SOPS**: Secret management with age/PGP encryption
- **Colmena**: Remote deployment tool for NixOS
- **Stylix**: System-wide theming based on base16
- **Git Hooks**: Pre-commit hooks for code quality

## Build Commands

The project uses `just` as a command runner. Key commands:

```bash
# System management
just build [ARGS]          # Build system configuration
just boot [ARGS]           # Build and set for next boot
just switch [ARGS]         # Build and switch to new generation
just home [ARGS]           # Manage home-manager configurations

# Development
just check [ARGS]          # Run flake checks
just lint                  # Run linters
just fix                   # Auto-fix linting issues
just fmt                   # Format code

# Theme management
just theme [kind]          # Switch system theme (light/dark)
just light                 # Switch to light theme
just dark                  # Switch to dark theme
```

## Architecture

### Features System
The `src/features/` directory contains modular configurations that can be enabled per host or user:
- Core system features (boot, networking, audio)
- Development tools (git, direnv, editors)
- Desktop environments (GNOME, Sway)
- Applications and services

### Host Configuration
Each host in `src/hosts/` defines:
- Hardware-specific settings
- Enabled features and modules  
- User assignments
- Deployment metadata

### User Management
User configurations in `src/users/` provide:
- Home Manager configurations
- User-specific feature sets
- Application preferences
- Development environments

## Current Hosts

- **tuvok**: MacBook Air M2 running NixOS via Asahi Linux (daily driver)
- **ryosuke**: Teenage Engineering Computer-1 mini-ITX desktop (HTPC/office)
- **boschic**: Gaming/workstation desktop with RTX 3080 Ti
- **hodgepodge**: Early-2014 MacBook Pro (retirement/testing)

## Development Environment

The project includes comprehensive development tooling:
- Pre-commit hooks for Nix linting and formatting
- Treefmt for consistent code formatting
- Statix for Nix static analysis
- Deadnix for unused code detection
- Git hooks for commit message validation

## Secret Management

Secrets are managed using SOPS with age encryption:
- Public keys stored in `ops/keys/`
- Encrypted secrets in `secrets/`
- Automatic key management via `.sops.yaml`

## Package Management

Custom packages are defined in `src/packages/` and overlays in `overlays/`. The configuration supports:
- Package overrides and patches
- Custom derivations
- Binary cache integration (Cachix)

## Testing

The `tests/` directory contains:
- NixOS VM tests
- Feature integration tests
- CI/CD validation

This configuration emphasizes modularity, reproducibility, and maintainability while providing a rich desktop and development environment across multiple machines.