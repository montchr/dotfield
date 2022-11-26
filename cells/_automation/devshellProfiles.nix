{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.cells.lib.dev) pkgWithCategory withCategory;
  inherit (nixpkgs.stdenv) isLinux;

  l = inputs.nixpkgs.lib // builtins;
  cats = cell.constants.devshellCategories;

  dotfield = pkgWithCategory cats.dotfield;
  maintenance = pkgWithCategory cats.maintenance;
  utils = pkgWithCategory cats.utils;

  commonCommands = [
    (utils nixpkgs.cachix)
    (utils nixpkgs.just)
    (utils nixpkgs.nix-diff)
    (utils nixpkgs.nix-tree)
    (utils nixpkgs.nvd)
    (withCategory cats.utils {
      name = "mozilla-addons-to-nix";
      help = "Generate a Nix package set of Firefox add-ons from a JSON manifest.";
      command = ''
        nix run sourcehut:~rycee/mozilla-addons-to-nix -- $@
      '';
    })

    (maintenance nixpkgs.alejandra)
    (maintenance nixpkgs.deadnix)
    (maintenance nixpkgs.nodePackages.prettier)
    (maintenance nixpkgs.statix)
    (maintenance nixpkgs.treefmt)
  ];

  linuxCommands = l.optionals isLinux [
    (dotfield inputs.deploy-rs.packages.deploy-rs)
    (dotfield inputs.nixos-generators.packages.nixos-generate)
  ];
in {
  default = _: {
    commands = commonCommands ++ linuxCommands;

    packages = [
      nixpkgs.cachix
      nixpkgs.editorconfig-checker
      nixpkgs.shellcheck
      nixpkgs.nodejs
      nixpkgs.nvfetcher
      nixpkgs.nodePackages.yarn
    ];
  };
}
