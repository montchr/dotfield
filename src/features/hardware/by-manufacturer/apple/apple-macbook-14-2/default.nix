flake@{ lib, ... }:
{
  dotfield.features.hardware__apple__macbook-14-2 = {
    nixos =
      { config, ... }:
      {
        imports = with flake.config.dotfield.features; [
          hardware__apple__apple-silicon.nixos
          hardware__apple__macbook.nixos
        ];

        config = lib.mkIf config.programs.sway.enable {
          environment.etc."sway/config".text = ''
            set $laptop eDP-1

            # output $laptop scale 2
            # output $laptop pos 0 0 res 2560x1600
          '';
        };
      };

    home = {
      services.kanshi.settings = [
        {
          output.criteria = "eDP-1";
          output.scale = 2.0;
        }
      ];
    };
  };
}
