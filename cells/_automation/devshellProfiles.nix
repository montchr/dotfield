{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.cells.lib.dev) pkgWithCategory;
  inherit (nixpkgs.stdenv) isLinux;

  l = inputs.nixpkgs.lib // builtins;
  cats = cell.constants.devshellCategories;

  dotfield = pkgWithCategory cats.dotfield;
  maintenance = pkgWithCategory cats.maintenance;
  utils = pkgWithCategory cats.utils;

  home-manager = inputs.home-manager.packages.default;

  commonCommands = [
    (utils nixpkgs.cachix)
    (utils home-manager)
    (utils nixpkgs.just)
    (utils nixpkgs.nix-diff)
    (utils nixpkgs.nix-tree)
    (utils nixpkgs.nvd)

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
    env = [
      {
        name = "DOTFIELD_SYS_DRV";
        eval = "/nix/var/nix/profiles/system";
      }
      {
        name = "DOTFIELD_HOME_DRV";
        eval = "/nix/var/nix/profiles/per-user/$USER/home-manager";
      }
    ];
    packages = [
      nixpkgs.cachix
      nixpkgs.editorconfig-checker
      nixpkgs.shellcheck
      nixpkgs.nodejs
      nixpkgs.nodePackages.yarn
    ];
  };
}
