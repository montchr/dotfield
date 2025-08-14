### LG-27GL850-B Monitor

{ self, ... }:
let
  name = "LG-27GL850-B";
in
{
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
  dotfield.meta.displays.${name}.dpi = 109;

  dotfield.features."hardware/lg/${name}".nixos = {
    services.xserver.dpi = self.dotfield.meta.displays.${name}.dpi;
  };

}
