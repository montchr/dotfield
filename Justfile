default: build

icon-ok := '✔'
msg-ok := icon-ok + " OK"
msg-done := icon-ok + " Done"

firefox-addons-dir := "${PRJ_ROOT}/packages/applications/firefox/firefox-addons"
sources-dir := "${PRJ_ROOT}/packages/sources"


###: LINTING/FORMATTING ========================================================

# Format source files
fmt *FILES="$PRJ_ROOT":
  treefmt --no-cache \
    --config-file $PRJ_ROOT/treefmt.toml \
    --tree-root $PRJ_ROOT \
    {{FILES}}

# Write automatic linter fixes to source files
fix *FILES="$PRJ_ROOT":
  treefmt --no-cache \
    --config-file $PRJ_ROOT/treefmt.lint-fix.toml \
    --tree-root $PRJ_ROOT \
    {{FILES}}


###: SYSTEM ====================================================================

cachix-cache-name := 'dotfield'
cachix-exec := "cachix watch-exec --jobs 2 " + cachix-cache-name

sys-rebuild := if os() == "linux" {
  "nixos-rebuild"
} else if os() == "macos" {
  "darwin-rebuild"
} else { "nix build" }

# Rebuild the system and push any new derivations to the binary cache
system subcommand="build" *ARGS='':
  {{cachix-exec}} {{sys-rebuild}} {{subcommand}} -- \
    {{ARGS}} --flake $PRJ_ROOT --verbose
  @echo {{msg-done}}

# Rebuild the system and provide a summary of the changes
build: (system "build")
  nvd diff /run/current-system $PRJ_ROOT/result

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
