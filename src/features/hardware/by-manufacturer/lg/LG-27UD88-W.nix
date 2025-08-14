### LG 27UD88-W Monitor

{ lib, self, ... }:

let
  name = "LG-27UD88-W";
in

{
  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 3840 2160;        => 4405.814340165
  # diagIn = 27;
  # ppi = diagPx / diagIn;          => 163.178308895
  #
  # source: https://www.calculatorsoup.com/calculators/technology/ppi-calculator.php
  dotfield.meta.displays.${name}.dpi = 163;

  dotfield.features."hardware/lg/${name}".nixos =
    { config, ... }:
    lib.mkMerge [
      { services.xserver.dpi = self.dotfield.displays.${name}.dpi; }
    ]
    ++ (lib.mkIf config.programs.sway.enable {
      # Output DVI-I-1 'LG Electronics LG Ultra HD 0x000668B9'
      #   Current mode: 3840x2160 @ 60.000 Hz
      #   Power: on
      #   Position: 1280,0
      #   Scale factor: 1.000000
      #   Scale filter: nearest
      #   Subpixel hinting: unknown
      #   Transform: normal
      #   Workspace: 1
      #   Max render time: off
      #   Adaptive sync: disabled
      #   Allow tearing: no
      #   Available modes:
      #     3840x2160 @ 60.000 Hz
      #     3840x2160 @ 60.000 Hz (16:9)
      #     3840x2160 @ 59.940 Hz (16:9)
      #     3840x2160 @ 50.000 Hz (16:9)
      #     3840x2160 @ 30.000 Hz
      #     3840x2160 @ 30.000 Hz (16:9)
      #     3840x2160 @ 29.970 Hz (16:9)
      #     3840x2160 @ 25.000 Hz (16:9)
      #     3840x2160 @ 24.000 Hz (16:9)
      #     3840x2160 @ 23.976 Hz (16:9)
      #     2560x1440 @ 59.951 Hz
      #     1920x1080 @ 60.000 Hz
      #     1920x1080 @ 60.000 Hz (16:9)
      #     1920x1080 @ 59.940 Hz (16:9)
      #     1920x1080 @ 30.000 Hz (16:9)
      #     1920x1080 @ 29.970 Hz (16:9)
      #     1920x1080 @ 24.000 Hz (16:9)
      #     1920x1080 @ 23.976 Hz (16:9)
      #     1600x900 @ 60.000 Hz
      #     1280x1024 @ 60.020 Hz
      #     1280x800 @ 59.910 Hz
      #     1152x864 @ 59.967 Hz
      #     1280x720 @ 60.000 Hz
      #     1280x720 @ 60.000 Hz (16:9)
      #     1280x720 @ 59.940 Hz (16:9)
      #     1024x768 @ 60.004 Hz
      #     800x600 @ 60.317 Hz
      #     720x480 @ 60.000 Hz (16:9)
      #     720x480 @ 60.000 Hz (4:3)
      #     720x480 @ 59.940 Hz (16:9)
      #     720x480 @ 59.940 Hz (4:3)
      #     640x480 @ 60.000 Hz (4:3)
      #     640x480 @ 59.940 Hz
      #     640x480 @ 59.940 Hz (4:3)
      environment.etc."sway/config".text = ''
        set $disp_lg_27ud88w DVI-I-1

        output $disp_lg_27ud88w scale 2
      '';
    });
}
