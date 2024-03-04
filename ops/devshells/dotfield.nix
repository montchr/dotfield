{self, ...}: let
  inherit (self.inputs) apparat;
  inherit (apparat.lib) enumAttrs;
  inherit (apparat.lib.devshell) pkg;
in {
  perSystem = {
    system,
    inputs',
    pkgs,
    ...
  }: let
    inherit (inputs') colmena home-manager;

    cats = enumAttrs [
      "dotfield"
      "general"
      "legal"
      "maintenance"
      "packaging"
      "utils"
    ];

    dotfield = pkg cats.dotfield;
    maintenance = pkg cats.maintenance;
  in {
    # devshells.dotfield = {
    devshells.default = {
      devshell.name = "dotfield";
      devshell.packages = [
        pkgs.alejandra
        pkgs.cachix
        pkgs.crudini # CRUD for ini files -- for dconf dump manipulation
        pkgs.deadnix
        pkgs.editorconfig-checker
        pkgs.gh
        pkgs.nix-diff
        pkgs.nix-output-monitor
        pkgs.nodePackages.prettier
        pkgs.reuse
        pkgs.shellcheck
        pkgs.statix
        pkgs.treefmt
      ];
      commands = [
        (dotfield colmena.packages.colmena)
        (dotfield home-manager.packages.default)

        (dotfield pkgs.just)
        (dotfield pkgs.treefmt)

        (maintenance pkgs.nix-init)
        (maintenance pkgs.nix-prefetch)
        (maintenance pkgs.nix-tree)
        (maintenance pkgs.nixdoc)
        (maintenance pkgs.nurl)
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
    };
  };
}
