{
  self,
  lib,
  ...
}:
{
  dotfield.features."hardware/laptop".nixos =
    { config, ... }:
    {
      imports = [
        self.dotfield.features."hardware/battery".nixos
        self.dotfield.features.networkmanager.nixos
      ];

      config = lib.mkIf config.programs.sway.enable {
        # NOTE: Requires that the $laptop variable is set!  This should be
        # added in a module specific to a laptop model, as its value may vary.
        environment.etc."sway/config".text = lib.mkAfter ''
          # Clamshell mode
          # <https://github.com/swaywm/sway/wiki#user-content-clamshell-mode>
          bindswitch --reload --locked lid:on output $laptop disable
          bindswitch --reload --locked lid:off output $laptop enable
        '';
      };
    };
}
