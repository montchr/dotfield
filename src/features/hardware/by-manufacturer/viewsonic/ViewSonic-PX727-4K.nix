### ViewSonic PX727-4K Projector

flake@{ ... }:
let
  name = "ViewSonic-PX727-4K";
in
{
  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 3840 2160;        => 4405.81
  # diagIn = ~100;
  # ppi = diagPx / diagIn;          => 44.06
  #
  # source: https://www.calculatorsoup.com/calculators/technology/ppi-calculator.php
  dotfield.meta.displays.${name}.dpi = 44;

  dotfield.aspects.hardware__viewsonic__PX727-4K.nixos = {
    services.xserver.dpi = flake.config.dotfield.meta.displays.${name}.dpi;
  };
}
