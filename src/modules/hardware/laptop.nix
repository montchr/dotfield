{
  self,
  lib,
  ...
}:
{
  dotfield.modules."hardware/laptop".nixos = {
    imports = [
      self.dotfield.modules."hardware/battery".nixos
      self.dotfield.modules.networkmanager.nixos
    ];
  };

  # TODO: this is not quite right... graphical does not imply it has a
  # lid.  workstation is too specific.  "tangible" is not necessarily graphical.
  dotfield.modules.graphical.nixos =
    { config, ... }:
    lib.mkIf config.programs.sway.enable {
      # NOTE: Requires that the $laptop variable is set!  This should be
      # added in a module specific to a laptop model, as its value may vary.
      environment.etc."sway/config".text = lib.mkAfter ''
        # Clamshell mode
        # <https://github.com/swaywm/sway/wiki#user-content-clamshell-mode>
        bindswitch --reload --locked lid:on output $laptop disable
        bindswitch --reload --locked lid:off output $laptop enable
      '';
    };
}
