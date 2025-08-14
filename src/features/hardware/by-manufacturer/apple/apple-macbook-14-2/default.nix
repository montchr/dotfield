{ lib, self, ... }:
{
  dotfield.features."hardware/apple/macbook-14-2".nixos = {
    imports = [
      self.dotfield.features."hardware/apple-silicon".nixos
      self.dotfield.features."hardware/apple-macbook".nixos
    ];
  };

  dotfield.features.graphical.nixos =
    { config, ... }:
    lib.mkIf config.programs.sway.enable {
      environment.etc."sway/config".text = ''
        set $laptop eDP-1

        # output $laptop scale 2
        # output $laptop pos 0 0 res 2560x1600
      '';
    };
}
