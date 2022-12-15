# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later

###: https://just.systems/man/en/

default:
  @just --list --unsorted --color=always | rg -v "    default"

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
# `package.json` runnables
export PATH := "./node_modules/.bin:" + env_var('PATH')

sys-gen-path := "/run/current-system"
hm-fragment := quote( env_var('USER') + '@' + `hostname` )
hm-gen-path := `home-manager generations | head -1 | grep -Eo '\/nix\/store.+$'`

sys-cmd := if os() == "linux" {
  "nixos-rebuild"
} else if os() == "macos" {
  "darwin-rebuild"
} else { "nix build" }


###: LINTING/FORMATTING ========================================================

# <- Lint and format files
fmt *FILES=prj-root:
  treefmt --no-cache {{FILES}}

# <- Write automatic linter fixes to files
fix *FILES=prj-root: (deadnix "fix" FILES) (statix "fix" FILES)

# <- Run `statix`
[private]
statix action +FILES=prj-root:
  @ # Note that stderr is silenced due to an upstream bug
  @ # https://github.com/nerdypepper/statix/issues/59
  @ for f in {{FILES}}; do \
    statix {{action}} -- "$f" 2>/dev/null; \
  done

# <- Run `deadnix` with sane options
[private]
deadnix action +FILES=prj-root:
  @deadnix \
    {{ if action == "fix" { "--edit" } else { "--fail" } }} \
    --no-underscore \
    --no-lambda-pattern-names \
    {{FILES}}


###: INTROSPECTION =============================================================

# <- Diff the current + next system generations.
diff-next-sys: (system "build")
  nix-diff "{{sys-gen-path}}" "{{prj-root}}/result"

# <- Diff the current + next home generations.
diff-next-home:
  nix-diff {{hm-gen-path}} \
    `nix build --print-out-paths {{prj-root}}#homeConfigurations.{{hm-fragment}}.activationPackage`


###: SYSTEM ====================================================================

# <- Rebuild the system and provide a summary of the changes
build *ARGS='': (system "build" ARGS)

# <- Rebuild the system and switch to the next generation
switch *ARGS='': (system "switch" ARGS)

# <- Rebuild a host and push any new derivations to the binary cache
system subcommand *ARGS='':
  {{cachix-exec}} {{sys-cmd}} {{subcommand}} -- \
    {{ARGS}} --flake "{{prj-root}}" --verbose
  @echo {{msg-done}}

###: HOME-MANAGER ==============================================================

# <- Rebuild a home configuration and push to the binary cache
home subcommand name=hm-fragment *ARGS='--impure':
  {{cachix-exec}} home-manager {{subcommand}} -- \
    {{ARGS}} --flake "{{prj-root}}" --verbose
  @echo {{msg-done}}


###: THEME =====================================================================

# TODO: <- Toggle all application themes between light<->dark

# <- Set the theme for all applications
set-theme colors='dark': && (set-system-appearance colors) (set-kitty-theme colors) (set-emacs-theme colors)
  DOTFIELD_COLORS="{{colors}}" home-manager switch --impure --flake "{{prj-root}}"

##: --- kitty ---

# <- Toggle the current kitty theme between light<->dark
[private]
set-kitty-theme mode='dark':
  @echo "Setting kitty {{mode}} theme"
  kitty @set-colors -a -c $KITTY_CONFIG_DIRECTORY/theme-{{mode}}.conf
  @echo {{msg-done}}

##: --- emacs ---

emacs-dark := "(modus-themes-load-vivendi)"
emacs-light := "(modus-themes-load-operandi)"

# FIXME: server not reachable, but should otherwise work
# <- Toggle the current Emacs theme between light<->dark
[private]
set-emacs-theme mode='dark':
  just _emacs-cmd {{ quote( if mode == 'dark' { emacs-dark } else { emacs-light } ) }}
_emacs-cmd eval:
  emacsclient --no-wait --eval "{{eval}}" >/dev/null


##: --- macOS ---

applescript-dark-mode := 'tell app "System Events" to tell appearance preferences to set dark mode to '
_mac-dark-mode value:
  osascript -e {{ quote( applescript-dark-mode + value ) }}

# <- Toggle the current system theme between light<->dark
[private]
[macos]
set-system-appearance mode="toggle":
  {{ if mode == "dark" { "just _mac-dark-mode 'true'" } else if mode == "light" { "just _mac-dark-mode 'false'" } else { "just _mac-dark-mode 'not dark mode'" } }}



###: UPDATES ===================================================================

update-all: update-flake-inputs update-sources update-firefox-addons update-doom

# <- Update the flake inputs
update-flake-inputs:
  @echo 'Updating flake inputs...'
  nix flake update --verbose
  @echo {{msg-done}}

# <- Update Doom Emacs
update-doom:
  @echo 'Updating Doom Emacs...'
  doom upgrade
  @echo {{msg-done}}

# <- Generate updated Nix expressions for Firefox addons
update-firefox-addons dir=firefox-addons-dir:
  @echo 'Updating Nix expressions for Firefox addons...'
  mozilla-addons-to-nix {{dir}}/addons.json {{dir}}/addons.generated.nix
  @echo {{msg-done}}

# <- Generate updated Nix expressions for external sources
update-sources dir=sources-dir:
  @echo 'Updating external sources with nvfetcher...'
  cd {{sources-dir}} && nvfetcher -c ./sources.toml
  @echo {{msg-done}}


###: LICENSING =================================================================

# <- Add the project default license header to the specified files
alias license := license-gpl

# <- Validate the project's licensing and copyright info
license-check:
  reuse lint

# <- Add a GPL-3.0-or-later license header to the specified files
license-gpl +FILES:
  reuse addheader -l {{default-license}} -c '{{copyright}}' {{FILES}}

# <- Add a CC-BY-SA-4.0 license header to the specified files
license-cc +FILES:
  reuse addheader -l {{docs-license}} -c '{{copyright}}' {{FILES}}

# <- Add a public domain CC0-1.0 license header to the specified files
license-public-domain +FILES:
  reuse addheader -l {{public-domain-license}} -c '{{copyright}}' {{FILES}}
