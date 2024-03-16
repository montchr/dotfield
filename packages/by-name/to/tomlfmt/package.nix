{ nodePackages, writeShellScriptBin }:
writeShellScriptBin "tomlfmt" ''
  ${nodePackages.prettier}/bin/prettier \
    --plugin "${nodePackages.prettier-plugin-toml}/lib/node_modules/prettier-plugin-toml/lib/index.js" \
    "$@"
''
