##: LG-27GL850-B Monitor
{
  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 2560 1440;        => 2937.21
  # diagIn = 27;
  # ppi = diagPx / diagIn;          => 108.79
  #
  # source: https://www.calculatorsoup.com/calculators/technology/ppi-calculator.php
  services.xserver.dpi = 109;
}
