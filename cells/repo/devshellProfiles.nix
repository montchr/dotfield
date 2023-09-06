{
  inputs,
  cell,
}: let
  inherit (inputs) apparat colmena home-manager nixpkgs namaka;
  inherit (apparat.lib.devshell) pkg pkg';

  l = inputs.nixpkgs.lib // builtins;
  cats = cell.constants.devshellCategories;

  dotfield = pkg cats.dotfield;
  maintenance = pkg cats.maintenance;
  utils' = pkg' cats.utils;

  pre-commit-hooks = import ./devshellProfiles/pre-commit-hooks.nix {inherit inputs cell;};
in {
  default = _: {
    commands = [
      (dotfield colmena.packages.colmena)
      (dotfield home-manager.packages.default)
      (dotfield namaka.packages.default)
      (dotfield nixpkgs.just)
      # (utils' "nom" nixpkgs.nix-output-monitor)
      (dotfield nixpkgs.treefmt)
      # (maintenance nixpkgs.treefmt)
    ];
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
      nixpkgs.alejandra
      nixpkgs.cachix
      nixpkgs.deadnix
      nixpkgs.editorconfig-checker
      nixpkgs.gh
      nixpkgs.nix-diff
      nixpkgs.nix-output-monitor
      nixpkgs.nodePackages.prettier
      nixpkgs.reuse
      nixpkgs.shellcheck
      nixpkgs.statix
      nixpkgs.treefmt
    ];
  };
  pre-commit-hooks = l.id pre-commit-hooks;
}
