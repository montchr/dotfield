##: ViewSonic PX727-4K Projector
{
  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 3840 2160;        => 4405.81
  # diagIn = ~100;
  # ppi = diagPx / diagIn;          => 44.06
  #
  # source: https://www.calculatorsoup.com/calculators/technology/ppi-calculator.php
  services.xserver.dpi = 44;
}
