{
  inputs,
  cell,
}: let
  inherit (inputs) home-manager nixos-generators nixpkgs namaka;
  inherit (inputs.cells.lib.dev) pkgWithCategory;
  inherit (inputs.nixpkgs.stdenv) isLinux;

  l = inputs.nixpkgs.lib // builtins;
  cats = cell.constants.devshellCategories;

  dotfield = pkgWithCategory cats.dotfield;
  maintenance = pkgWithCategory cats.maintenance;
  utils = pkgWithCategory cats.utils;

  commonCommands = [
    (dotfield home-manager.packages.default)
    (dotfield namaka.packages.default)
    (dotfield nixpkgs.just)

    (utils nixpkgs.cachix)
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
    (dotfield nixos-generators.packages.nixos-generate)
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
        eval = "\${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/home-manager";
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
