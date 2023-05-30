# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later

###: https://just.systems/man/en/

default:
  @just --list --unsorted --color=always | rg -v "\s*default"

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
# `package.json` runnables
export PATH := "./node_modules/.bin:" + env_var('PATH')

sys-gen-path := env_var('DOTFIELD_SYS_DRV')
hm-gen-path := env_var('DOTFIELD_HOME_DRV')
hm-fragment := quote( env_var('USER') + '@' + `hostname` )

sys-cmd := if os() == "linux" {
  "nixos-rebuild"
} else if os() == "macos" {
  "darwin-rebuild"
} else { "nix build" }


###: UTILITIES =================================================================

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

# <- Diff the current + next system generations.
diff-next-sys: (system "build")
  nix-diff "{{sys-gen-path}}" "{{prj-root}}/result"
  @echo {{msg-done}}

# <- Diff the current + next home generations.
diff-next-home:
  nix-diff {{hm-gen-path}} \
    `nix build --print-out-paths {{prj-root}}#homeConfigurations.{{hm-fragment}}.activationPackage`
  @echo {{msg-done}}


###: SYSTEM ====================================================================

# <- Rebuild the system and provide a summary of the changes
build *ARGS='':
  {{cachix-exec}} {{sys-cmd}} build -- \
    {{ARGS}} --flake "{{prj-root}}" --verbose
  @echo {{msg-done}}

# <- Rebuild the system and switch to the next generation
switch *ARGS='': (system "switch" ARGS)

# <- Rebuild a host and push any new derivations to the binary cache
system subcommand *ARGS='':
  {{sys-cmd}} {{subcommand}} \
    {{ARGS}} --flake "{{prj-root}}" --verbose
  @echo {{msg-done}}


###: HOME-MANAGER ==============================================================

# <- Rebuild a home configuration and push to the binary cache
home subcommand name=hm-fragment *ARGS='--impure':
  home-manager {{subcommand}} \
    {{ARGS}} --flake "{{prj-root}}" --verbose
  @echo {{msg-done}}


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

# <- Set the theme for all applications
theme colors='dark': && (set-system-appearance colors) (set-emacs-theme colors)
  DOTFIELD_COLORS="{{colors}}" home-manager switch --impure --flake "{{prj-root}}"
  @echo {{msg-done}}

# <- Use the 'light' theme for all applications
light: (theme "light")

# <- Use the 'dark' theme for all applications
dark: (theme "dark")

##: --- kitty ---

# FIXME: need to re-symlink files
# <- Switch the current kitty theme
# set-kitty-theme name='dark':
#   @echo "Setting kitty '{{name}}' theme"
#   kitty @set-colors -a -c $KITTY_CONFIG_DIRECTORY/theme-{{name}}.conf
#   @echo {{msg-done}}


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
