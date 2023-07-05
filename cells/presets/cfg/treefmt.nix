# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# FIXME: remove all linters b/c out of scope and complicated
# FIXME: format only
{
  global.excludes = [
    "*generated*"
    "*secrets.yaml"
    "*secrets.yml"
  ];
  formatter = {
    nix = {
      command = "sh";
      options = [
        "-euc"
        ''
          just statix check "$@"
          just deadnix check "$@"
          alejandra "$@"
        ''
        "--"
      ];
      includes = ["*.nix"];
      excludes = [
        ##: follows home-manager upstream formatting
        "home/modules/syncthing.nix"
      ];
    };
    js = {
      command = "sh";
      options = [
        "-eucx"
        ''
          prettier --write "$@"
          eslint "$@"
        ''
        "--"
      ];
      includes = [
        "*.cjs"
        "*.js"
        "*.jsx"
        "*.mjs"
        "*.ts"
        "*.tsx"
      ];
    };
    lua = {
      command = "stylua";
      includes = ["*.lua"];
    };
    prettier = {
      command = "prettier";
      options = ["--plugin" "prettier-plugin-toml" "--write"];
      includes = [
        "*.css"
        "*.html"
        "*.json"
        "*.md"
        "*.mdx"
        "*.scss"
        "*.toml"
        "*.yaml"
        "*.yml"
      ];
      excludes = [
        "dark-reader.json"
        "devdocs.json"
        "karabiner.json"
      ];
    };
    shell = {
      command = "shfmt";
      # shfmt will read settings from `.editorconfig` as long as no parser or
      # printer flags are passed. See `man shfmt` for details.
      options = ["--simplify" "--write"];
      includes = ["*.sh" "*.bash"];
    };
  };
}
