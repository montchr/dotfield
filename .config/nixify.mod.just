alias yml := yaml

data-dir := env("PRJ_DATA_HOME")
dconf-dump-tmpfile := data-dir / "dconf.settings"
dconf-output-file := data-dir / "dconf.settings.nix"

[no-cd]
all ext input-dir output-dir:
  fd -t f -e {{ ext }} . {{ input-dir }} -x bash -c \
    'just nixify {{ ext }} {} | nixfmt > {{ output-dir }}/{/.}.nix'

[doc("Convert a YAML file to a Nix expression")]
yaml file: (_yaml-to-json file)
  just nixify json {{ data-dir }}/{{ file_stem(file) }}.json

# FIXME: which yq?!
[no-cd]
[doc("Convert a YAML file to JSON with yq")]
_yaml-to-json file:
  yq '.' {{ file }} > {{ env("PRJ_DATA_HOME") / file_stem(file) }}.json

# TODO: read from stdin... how?
# builtins.fromJSON ''
#  {{ stdin }}
# ''
[no-cd]
[doc("Convert a JSON file to a Nix expression")]
json file:
  nix eval --expr 'builtins.fromJSON (builtins.readFile {{ file }})' --impure

[no-cd]
[doc("Convert a TOML file to a Nix expression")]
toml file:
  nix eval --expr 'builtins.fromTOML (builtins.readFile {{ file }})' --impure

# <https://github.com/nix-community/dconf2nix>
[no-cd]
[doc("Export current dconf/GNOME/GTK/gsettings as a Nix file")]
dconf: _dconf-dump
  dconf2nix -i {{ dconf-dump-tmpfile }} -o {{ dconf-output-file }}
  bat --style=plain --paging=never {{ dconf-output-file }}
  @echo 'Exported to {{ dconf-output-file }}'

_dconf-dump:
  # This `app-picker-layout` setting causes parse errors.
  # TODO: link to the dconf2nix issue
  dconf dump / | sed '/app-picker-layout/d' > {{ dconf-dump-tmpfile }}
