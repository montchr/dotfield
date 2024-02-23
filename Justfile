# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later

###: https://just.systems/man/en/

default:
  @just --choose

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
cachix-jobs := '4'
cachix-exec := "cachix watch-exec " + cachix-cache-name + " --jobs " + cachix-jobs

##: directories/paths
prj-root := env_var('PRJ_ROOT')
prj-data := env_var('PRJ_DATA_HOME')

sys-gen-path := env_var('DOTFIELD_SYS_DRV')
# FIXME: works on darwin, broken on linux due to stray backslashes before slashes
# hm-gen-path := `home-manager generations | head -1 | grep -Eo '\/nix\/store.+$'`
#
# FIXME: when running just with sudo, no generations available:
# ls: cannot access 'home-manager-*-link': No such file or directory
# TODO: does this work on darwin? it *should*, because we *should not* use different grep commands...
hm-gen-path := `home-manager generations | head -1 | grep -Eo '/nix/store.+$'`
hm-specialisation-path := hm-gen-path / "specialisations"
hm-fragment := quote( env_var('USER') + '@' + `hostname` )

sys-cmd := if os() == "linux" {
  "nixos-rebuild"
} else if os() == "macos" {
  "darwin-rebuild"
} else { "nix build" }


###: UTILITIES =================================================================

# TODO: allow raw input (needs multiline string format but it works in repl):
# builtins.fromJSON ''
#  {"foo": "bar"}
# ''

# <- Convert a JSON file to a Nix expression
json2nix file:
  nix eval --expr 'builtins.fromJSON (builtins.readFile {{file}})' --impure

# <- Convert a TOML file to a Nix expression
toml2nix file:
  nix eval --expr 'builtins.fromTOML (builtins.readFile {{file}})' --impure


###: LINTING/FORMATTING ========================================================

# <- Lint files
lint *FILES=prj-root: (deadnix "check" FILES) (statix "check" FILES)

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

check *ARGS:
    nix flake check --verbose {{ ARGS }}

# <- Diff the current + next system generations.
diff-next-sys: (system "build")
  nvd diff "{{sys-gen-path}}" "{{prj-root}}/result"
  @echo {{msg-done}}

# <- Diff the current + next home generations.
diff-next-home:
  nvd diff {{hm-gen-path}} \
    `nix build --print-out-paths {{prj-root}}#homeConfigurations.{{hm-fragment}}.activationPackage`
  @echo {{msg-done}}


###: SYSTEM ====================================================================

# <- Rebuild the system and provide a summary of the changes
build *ARGS='':
  {{cachix-exec}} {{sys-cmd}} build -- \
    {{ARGS}} --flake "{{prj-root}}"
  @echo {{msg-done}}

# FIXME: fails on nixos without sudo
# <- Rebuild the system and switch to the next generation
switch *ARGS='': (system "switch" ARGS)

# <- Rebuild a host and push any new derivations to the binary cache
system subcommand='build' *ARGS='':
  {{sys-cmd}} {{subcommand}} \
    {{ARGS}} --flake "{{prj-root}}"
  @echo {{msg-done}}


###: HOME-MANAGER ==============================================================

# FIXME: it seems that running just home switch does nothing on nixos?

# <- Run the home-manager CLI for the project flake.
home subcommand='build' *ARGS='':
  home-manager {{subcommand}} --flake "{{prj-root}}" {{ARGS}}
  @echo {{msg-done}}

home-specialise name:
  {{ hm-gen-path }}/specialisation/{{ name }}/activate


###: EMACS =====================================================================

emacs-load-theme-dark := "(load-theme 'modus-vivendi-tinted t)"
emacs-load-theme-light := "(load-theme 'modus-operandi-tinted t)"

# <- Evaluate elisp via `emacsclient`
emacs-eval elisp:
  emacsclient --no-wait --eval "{{elisp}}" >/dev/null

# Toggle the current Emacs theme between light<->dark
[private]
set-emacs-theme mode='dark': (emacs-eval if mode == 'dark' { emacs-load-theme-dark } else { emacs-load-theme-light } )


###: THEME =====================================================================

# <https://nix-community.github.io/home-manager/options.html#opt-specialisation>
# This is safe to use even with a dirty working tree because the themes are
# activated by way of the current generation's specialisation activation scripts.

# NOTE: home-manager requires re-activating a base generation before loading a specialization.
#       A specialisation is not available within a specialisation.
#       This is an upstream limitation:
#       <https://github.com/nix-community/home-manager/issues/4073>
# <- Set the theme for all applications
theme kind='dark': && (home-specialise kind) (set-system-appearance kind) (set-emacs-theme kind)

# <- Use the 'light' theme for all applications
light: (theme "light")

# <- Use the 'dark' theme for all applications
dark: (theme "dark")

##: --- kitty ---

# <- Switch the current kitty theme
set-kitty-theme kind='dark':
  @echo "Setting kitty '{{ kind }}' theme"
  kitty @set-colors -a -c $KITTY_CONFIG_DIRECTORY/theme-{{ kind }}.conf
  @echo {{ msg-done }}


##: --- gtk ---

gtk-ui-schema := "org.gnome.desktop.interface"

[linux]
colors-gtk command='get' kind='':
  gsettings {{ command }} {{ gtk-ui-schema }} color-scheme \
    {{ if command == 'set' { "prefer-" + kind } else { '' } }}

# FIXME: rename -- gtk is not a "system" - it's a ui framework
# <- Switch the current GTK theme kind (default: dark)
[private]
[linux]
set-system-appearance kind="dark": (colors-gtk "set" kind)


##: --- macOS ---

applescript-dark-mode := 'tell app "System Events" to tell appearance preferences to set dark mode to '

_mac-dark-mode value:
  osascript -e {{ quote( applescript-dark-mode + value ) }}


# <- Toggle the current system theme between light<->dark
[private]
[macos]
set-system-appearance mode="toggle":
  {{ if mode == "dark" { "just _mac-dark-mode 'true'" } else if mode == "light" { "just _mac-dark-mode 'false'" } else { "just _mac-dark-mode 'not dark mode'" } }}


###: LICENSING =================================================================

# <- Validate the project's licensing and copyright info
license-check:
  reuse lint

# <- Add a GPL-3.0-or-later license header to the specified files
license-gpl +FILES:
  reuse annotate -l {{default-license}} -c '{{copyright}}' {{FILES}}

# <- Add a CC-BY-SA-4.0 license header to the specified files
license-cc +FILES:
  reuse annotate -l {{docs-license}} -c '{{copyright}}' {{FILES}}

# <- Add a public domain CC0-1.0 license header to the specified files
license-public-domain +FILES:
  reuse annotate -l {{public-domain-license}} -c '{{copyright}}' {{FILES}}
