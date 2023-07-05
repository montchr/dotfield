{
  inputs,
  cell,
}: let
  inherit (inputs) apparat attic colmena home-manager nixos-generators nixpkgs namaka;
  inherit (apparat.lib.devshell) pkg pkg';
  inherit (nixpkgs.stdenv) isLinux;

  l = inputs.nixpkgs.lib // builtins;
  cats = cell.constants.devshellCategories;

  dotfield = pkg cats.dotfield;
  dotfield' = pkg' cats.dotfield;
  maintenance = pkg cats.maintenance;
  packaging = pkg cats.packaging;
  utils = pkg cats.utils;
  utils' = pkg' cats.utils;

  commonCommands = [
    (dotfield colmena.packages.colmena)
    (dotfield home-manager.packages.default)
    (dotfield namaka.packages.default)
    (dotfield nixpkgs.just)
    (dotfield' "deps" nixpkgs.nix-melt)
    (dotfield attic.packages.attic)

    (utils nixpkgs.cachix)
    (utils nixpkgs.nix-diff)
    (utils nixpkgs.nix-tree)
    (utils nixpkgs.nvd)
    (utils' "nom" nixpkgs.nix-output-monitor)

    (packaging nixpkgs.nix-init)

    (maintenance nixpkgs.alejandra)
    (maintenance nixpkgs.deadnix)
    (maintenance nixpkgs.nodePackages.prettier)
    (maintenance nixpkgs.statix)
    (maintenance nixpkgs.treefmt)
  ];

  linuxCommands = l.optionals isLinux [
    (dotfield nixos-generators.packages.nixos-generate)
  ];

  pre-commit-hooks = import ./devshellProfiles/pre-commit-hooks.nix {inherit inputs cell;};
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
        # alternatively: `home-manager generations | head -1 | grep -Eo '\/nix\/store.+$'`
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
  pre-commit-hooks = l.id pre-commit-hooks;
}
