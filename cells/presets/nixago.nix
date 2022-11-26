# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.std) std;
  inherit (inputs.cells) lib;
  l = inputs.nixpkgs.lib // builtins;
in {
  commitlint = lib.nixago.commitlint {
    configData = import ./nixago/commitlint.nix;
  };
  lefthook = std.nixago.lefthook {
    configData = import ./nixago/lefthook.nix;
  };
  prettier = lib.nixago.prettier {
    configData = import ./nixago/prettier.nix;
  };
  statix = lib.nixago.statix {
    configData = import ./nixago/statix.nix;
  };
  treefmt = std.nixago.treefmt {
    configData = import ./nixago/treefmt.nix;
    packages = [
      nixpkgs.alejandra
      nixpkgs.deadnix
      nixpkgs.nodePackages.eslint
      nixpkgs.nodePackages.prettier
      nixpkgs.nodePackages.prettier-plugin-toml
      nixpkgs.shfmt
      nixpkgs.statix
    ];
    devshell.startup.prettier-plugin-toml = l.stringsWithDeps.noDepEntry ''
      export NODE_PATH=${nixpkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
    '';
  };
}
