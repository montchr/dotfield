{ self, ... }:
let
  inherit (self.inputs) apparat;
  inherit (apparat.lib) enumAttrs;
  inherit (apparat.lib.devshell) pkg;
in
{
  perSystem =
    {
      inputs',
      config,
      pkgs,
      ...
    }:
    let
      inherit (inputs')
        colmena
        home-manager
        nix-inspect
        ;

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
    in
    {
      # devshells.dotfield = {
      devshells.default = {
        devshell.name = "dotfield";
        devshell.packages = (import ./__common-packages.nix pkgs) ++ [
          nix-inspect.packages.default

          pkgs.crudini # CRUD for ini files -- for dconf dump manipulation
          pkgs.gh
          pkgs.nh
          pkgs.nix-diff
          pkgs.nix-output-monitor
          pkgs.stow
        ];
        commands = [
          (dotfield colmena.packages.colmena)
          (dotfield home-manager.packages.default)

          # (maintenance (nix-inspect.packages.default.overrideAttrs { meta.description = "test"; }))

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
