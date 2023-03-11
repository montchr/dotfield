# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
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
    data = import ./nixago/commitlint.nix;
  };
  editorconfig = std.nixago.editorconfig {
    data = import ./nixago/editorconfig.nix;
  };
  lefthook = std.nixago.lefthook {
    data = import ./nixago/lefthook.nix;
  };
  prettier = lib.nixago.prettier {
    data = import ./nixago/prettier.nix;
  };
  statix = lib.nixago.statix {
    data = import ./nixago/statix.nix;
  };
  stylua = lib.nixago.stylua {
    data = import ./nixago/stylua.nix;
  };
  treefmt = std.nixago.treefmt {
    data = import ./nixago/treefmt.nix;
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
