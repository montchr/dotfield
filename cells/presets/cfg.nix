# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs std;
  inherit (inputs.cells) lib;
  l = inputs.nixpkgs.lib // builtins;
in {
  commitlint = lib.cfg.commitlint {
    data = import ./cfg/commitlint.nix;
  };
  editorconfig = std.lib.cfg.editorconfig {
    data = import ./cfg/editorconfig.nix;
  };
  lefthook = std.lib.cfg.lefthook {
    data = import ./cfg/lefthook.nix;
  };
  prettier = lib.cfg.prettier {
    data = import ./cfg/prettier.nix;
  };
  statix = lib.cfg.statix {
    data = import ./cfg/statix.nix;
  };
  stylua = lib.cfg.stylua {
    data = import ./cfg/stylua.nix;
  };
  treefmt = std.lib.cfg.treefmt {
    data = import ./cfg/treefmt.nix;
    packages = [
      nixpkgs.alejandra
      nixpkgs.deadnix
      nixpkgs.nodePackages.eslint
      nixpkgs.nodePackages.prettier
      nixpkgs.nodePackages.prettier-plugin-toml
      nixpkgs.shfmt
      nixpkgs.statix
      nixpkgs.stylua
    ];
    devshell.startup.prettier-plugin-toml = l.stringsWithDeps.noDepEntry ''
      export NODE_PATH=${nixpkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
    '';
  };
}
