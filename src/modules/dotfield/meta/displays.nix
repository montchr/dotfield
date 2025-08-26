{ inputs, lib, ... }:
let
  inherit (builtins)
    add
    div
    head
    tail
    ;
  inherit (inputs.nix-math.lib.math) pow sqrt;
  inherit (lib) mkOption types;

  diagPx = w: h: (sqrt (add (pow w 2) (pow h 2)));
  ppi =
    w: h: x:
    div x (diagPx w h);

  displaySubmodule =
    { config, ... }:
    {
      options = {
        deviceName = mkOption {
          type = types.str;
          description = "Full name of the device including manufacturer and model as output by `wayland-info` or `swaymsg -t get_outputs`.";
        };
        diag = mkOption {
          type = with types; float;
          description = "Diagonal width of the display";
        };
        # TODO: calculate this by also accepting an option for the true
        # diagonal dimensions of the display. or recommend an
        # authoritative reference source rather than taking the
        # manufacturer's advertised e.g. "27 inches" at its word (it's
        # never exactly that measurement, which could explain subpixel
        # offset in emacs?)
        dpi = mkOption {
          type = with types; nullOr int;
          default = null;
          description = "Pixel density of the display device.";
        };
        location = mkOption {
          type = with types; listOf str;
          default = [ ];
          description = "Qualitative tags describing the device's location from most- to least-specific.";
          example = lib.literalExpression ''[ "desk" "office" "home" ]'';
        };
        mode = mkOption {
          type = with types; listOf int;
          default = [ ];
          description = "Preferred display resolution dimensions in pixels as two integers.";
          example = lib.literalExpression "[ 2560 1440 ]";
        };
        scale = mkOption {
          type = types.float;
          default = 1.0;
          description = "Scaling factor for the video output";
        };
      };

      # FIXME: infinite recursion
      # config = lib.mkIf (config.dpi != null) {
      #   dpi = ppi (head config.mode) (tail config.mode) config.diag;
      # };
    };
in
{
  options.dotfield.meta.displays = mkOption {
    type = types.attrsOf (types.submodule displaySubmodule);
    default = { };
    description = "Displays metadata";
  };
}
