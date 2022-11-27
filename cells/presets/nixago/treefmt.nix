# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
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
        "**/_sources/*"
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
      excludes = ["_sources/**"];
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
