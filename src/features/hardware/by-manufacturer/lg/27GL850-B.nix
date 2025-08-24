### LG-27GL850-B Monitor

flake@{ ... }:
let
  cfg = flake.config.dotfield.meta.displays."LG-27GL850-B";
in
{
  dotfield.meta.displays."LG-27GL850-B" = {
    deviceName = "LG Electronics LG ULTRAGEAR 107NTBKA5869";
    mode = [
      2560
      1440
    ];
    scale = 1.0;

    # diag = w: h: sqrt(w^2 + h^2);
    # diagPx = diag 2560 1440;        => 2937.21
    # diagIn = 27;
    # ppi = diagPx / diagIn;          => 108.79
    #
    # source: https://www.calculatorsoup.com/calculators/technology/ppi-calculator.php
    #
    # FIXME: this seems low...?  this display is pretty high res,
    # definitely not so low in comparison to the other LG display in this
    # directory.  it might explain why, iirc, i've had to drastically
    # alter the font size in Emacs?
    dpi = 109;

    location = [
      "desk"
      "office"
      "home"
    ];
  };

  dotfield.aspects.hardware__lg__27GL850-B = {
    nixos = {
      services.xserver.dpi = cfg.dpi;
    };
    home = {
      services.kanshi.settings = [
        {
          output = {
            inherit (cfg) scale;
            criteria = cfg.deviceName;
            mode = builtins.concatStringsSep "x" cfg.mode;
          };
        }
      ];
    };
  };

}
