# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later

default: build

##: feedback
icon-ok := '✔'
msg-ok := icon-ok + " OK"
msg-done := icon-ok + " Done"

##: legal/reuse
copyright := 'Chris Montgomery <chris@cdom.io>'
default-license := 'GPL-3.0-or-later'
docs-license := 'CC-BY-SA-4.0'
public-domain-license := 'CC0-1.0'

##: binary cache
cachix-cache-name := 'dotfield'
cachix-exec := "cachix watch-exec --jobs 2 " + cachix-cache-name

##: directories/paths
prj-root := env_var('PRJ_ROOT')
firefox-addons-dir := prj-root / "packages/applications/firefox/firefox-addons"
sources-dir := prj-root / "packages/sources"


###: LINTING/FORMATTING ========================================================

# Lint and format files
fmt *FILES=prj-root:
  treefmt --no-cache {{FILES}}

# Write automatic linter fixes to files
lint-fix *FILES=prj-root: (deadnix "fix" FILES) (statix "fix" FILES)

# Run `statix`
statix action +FILES=prj-root:
  @ # Note that stderr is silenced due to an upstream bug
  @ # https://github.com/nerdypepper/statix/issues/59
  @ for f in {{FILES}}; do \
    statix {{action}} -- "$f" 2>/dev/null; \
  done

# Run `deadnix` with sane options
deadnix action +FILES=prj-root:
  @deadnix \
    {{ if action == "fix" { "--edit" } else { "--fail" } }} \
    --no-underscore \
    --no-lambda-pattern-names \
    {{FILES}}


###: SYSTEM ====================================================================

sys-rebuild := if os() == "linux" {
  "nixos-rebuild"
} else if os() == "macos" {
  "darwin-rebuild"
} else { "nix build" }

# Rebuild the system and push any new derivations to the binary cache
system subcommand="build" *ARGS='':
  {{cachix-exec}} {{sys-rebuild}} {{subcommand}} -- \
    {{ARGS}} --flake {{prj-root}} --verbose
  @echo {{msg-done}}

# Rebuild the system and provide a summary of the changes
build: (system "build")
  nvd diff /run/current-system {{prj-root}}/result

# Rebuild the system and switch to the next generation
switch: (system "switch")


###: UPDATES ===================================================================

update-all: update-flake-inputs update-sources update-firefox-addons update-doom

# Update the flake inputs
update-flake-inputs:
  @echo 'Updating flake inputs...'
  nix flake update --verbose
  @echo {{msg-done}}

# Update Doom Emacs
update-doom:
  @echo 'Updating Doom Emacs...'
  doom upgrade
  @echo {{msg-done}}

# Generate updated Nix expressions for Firefox addons
update-firefox-addons dir=firefox-addons-dir:
  @echo 'Updating Nix expressions for Firefox addons...'
  mozilla-addons-to-nix {{dir}}/addons.json {{dir}}/addons.nix
  @echo {{msg-done}}

# Generate updated Nix expressions for external sources
update-sources dir=sources-dir:
  @echo 'Updating external sources with nvfetcher...'
  cd {{sources-dir}} && nvfetcher -c ./sources.toml
  @echo {{msg-done}}


###: APPEARANCE ================================================================

# Toggle the current kitty theme between light<->dark
set-kitty-theme mode='dark':
  @echo "Setting kitty terminal colors to {{mode}} scheme"
  kitty @set-colors -a -c $KITTY_CONFIG_DIRECTORY/themes/{{mode}}.conf
  @echo {{msg-done}}

# Toggle the current Emacs theme between light<->dark
toggle-emacs-theme:
  @echo "Toggling Emacs modus-themes color scheme"
  emacsclient --no-wait --eval "(modus-themes-toggle)" >/dev/null
  @echo {{msg-done}}

# Toggle the current macOS system theme between light<->dark
[macos]
toggle-macos-appearance:
  @echo "Toggling macOS system appearance mode"
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
  @echo {{msg-done}}

# Toggle all application themes between light<->dark
[macos]
toggle-ui-mode: toggle-emacs-theme toggle-macos-appearance
  @echo {{msg-done}}


###: LICENSING =================================================================

# Add the project default license header to the specified files
alias license := license-gpl

# Validate the project's licensing and copyright info
license-check:
  reuse lint

# Add a GPL-3.0-or-later license header to the specified files
license-gpl +FILES:
  reuse addheader -l {{default-license}} -c '{{copyright}}' {{FILES}}

# Add a CC-BY-SA-4.0 license header to the specified files
license-cc +FILES:
  reuse addheader -l {{docs-license}} -c '{{copyright}}' {{FILES}}

# Add a public domain CC0-1.0 license header to the specified files
license-public-domain +FILES:
  reuse addheader -l {{public-domain-license}} -c '{{copyright}}' {{FILES}}
