# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chmont@proton.me>
# SPDX-License-Identifier: GPL-3.0-or-later

###: https://just.systems/man/en/

default:
  @just --choose

##: legal/reuse
copyright := 'Chris Montgomery <chmont@proton.me>'
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

hm-gen-path := `home-manager generations | head -1 | grep -Eo '/nix/store.+$'`
hm-specialisation-path := hm-gen-path / "specialisations"
hm-fragment := quote( env_var('USER') + '@' + `hostname` )

# <- Rebuild the system and provide a summary of the changes
build *ARGS='':
  {{cachix-exec}} nh -- os build "{{prj-root}}" {{ARGS}}

# <- Rebuild the system and switch to the next generation
switch *ARGS='':
  {{cachix-exec}} nh -- os switch "{{prj-root}}" {{ARGS}}

home args:
  {{cachix-exec}} nh -- home {{prj-root}} {{args}}

home-specialise name:
  nh home switch {{prj-root}} -s {{name}}

# <- Run flake checks
check *ARGS:
    nix flake check --verbose {{ ARGS }}

# <- Inspect flake outputs
inspect:
  nix-inspect

# <- Lint files
lint: (_deadnix '--fail')
  statix check

# <- Write linter fixes to files
fix: (_deadnix "--edit")
  statix fix

# <- Lint and format files
fmt *FILES=prj-root:
  treefmt --no-cache {{FILES}}

_deadnix method='--fail' *ARGS='--no-underscore':
  fd -t f -e nix --exclude='packages/**/*.nix' --exec-batch \
    deadnix {{method}} {{ARGS}}
  fd -t f -e nix . packages --exec-batch \
    deadnix {{method}} --no-lambda-pattern-names {{ARGS}}

# <- Generate a Nix package expression from a URL
init-package pname url:
  nix-init --url {{url}} packages/{{pname}}/package.nix
  @echo "Add this to packages/default.nix:"
  @echo '{{pname}} = callPackage ./{{pname}}/package.nix { };'

###: GENERATE/CONVERT =================================================================

# <- Generate a hashed password compatible with the NixOS options
generate-hashed-password:
  mkpasswd -m sha-512

# <- Convert a YAML file to a Nix expression
nixify-yaml file:
  just nixify-json <(yq '.' {{file}})

# or, without recursive just:
# nixify-yaml file: && (nixify-json prj-data / file_stem(file) + ".json")
#   yq '.' '{{file}}' > {{prj-data / file_stem(file)}}.json

# TODO: allow raw input (needs multiline string format but it works in repl):
# builtins.fromJSON ''
#  {"foo": "bar"}
# ''
# <- Convert a JSON file to a Nix expression
nixify-json file:
  nix eval --expr 'builtins.fromJSON (builtins.readFile {{file}})' --impure

# <- Convert a TOML file to a Nix expression
nixify-toml file:
  nix eval --expr 'builtins.fromTOML (builtins.readFile {{file}})' --impure

# <https://github.com/nix-community/dconf2nix>
# <- Export current dconf/GNOME/GTK/gsettings as INI and Nix files
nixify-dconf out=prj-data:
  dconf dump / > {{out}}/dconf.settings
  dconf2nix \
    -i {{out}}/dconf.settings \
    -o {{out}}/dconf.settings.nix
  bat --style=plain --paging=never \
    {{out}}/dconf.settings.nix
  @echo 'Exported to {{out}}'

###: THEME =====================================================================

emacs-eval-cmd := "emacsclient --no-wait --eval"
kitty-set-colors-cmd := "kitty @set-colors -a -c"
gtk-ui-schema := "org.gnome.desktop.interface"

# <- Set the theme for all applications
[linux]
theme kind='dark': && (wm-set-theme kind) (kitty-set-theme kind)
  nh home switch {{prj-root}} -s {{kind}}
  {{emacs-eval-cmd}} '(ceamx/load-{{kind}}-theme)'

# <- Use the 'light' theme for all applications
light: (theme "light")

# <- Use the 'dark' theme for all applications
dark: (theme "dark")

# <- Switch the current kitty theme
kitty-set-theme kind='dark':
  @echo "Setting kitty '{{ kind }}' theme"
  {{kitty-set-colors-cmd}} $KITTY_CONFIG_DIRECTORY/theme-{{ kind }}.conf

[private]
[linux]
gtk-theme command='get' kind='':
  gsettings {{ command }} {{ gtk-ui-schema }} color-scheme \
    {{ if command == 'set' { "prefer-" + kind } else { '' } }}

# <- Switch the current GTK theme kind [default: dark]
[private]
[linux]
wm-set-theme kind="dark": (gtk-theme "set" kind)

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
