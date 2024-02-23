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
      (dotfield nixpkgs.just)
      (dotfield nixpkgs.treefmt)

      (maintenance nixpkgs.nix-init)
      (maintenance nixpkgs.nix-prefetch)
      (maintenance nixpkgs.nix-tree)
      (maintenance nixpkgs.nixdoc)
      (maintenance nixpkgs.nurl)
    ];
    env = [
      {
        name = "DOTFIELD_SYS_DRV";
        eval = "/nix/var/nix/profiles/system";
      }
      {
        name = "DOTFIELD_HOME_DRV";
        # FIXME: cannot switch to specialisations twice -- needs a full rebuild
        # before another specialization can be loaded.
        #
        # alternatively: `home-manager generations | head -1 | grep -Eo
        # '\/nix\/store.+$'` (this points to the same profile, however)
        #
        # note that the command above will only act on the most recent
        # generation (`head -1`) -- the issue then is that switching to a
        # specialisation creates a new generation, thus making that generation
        # the most recent. so then we need to determine whether a generation is
        # a "full" generation or merely a specialisation generation.
        eval = "\${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/home-manager";
      }
    ];
    packages = [
      nixpkgs.alejandra
      nixpkgs.cachix
      nixpkgs.crudini # CRUD for ini files -- for dconf dump manipulation
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
