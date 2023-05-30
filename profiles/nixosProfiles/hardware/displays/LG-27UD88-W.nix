##: LG 27UD88-W Monitor
{
  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 3840 2160;        => 4405.814340165
  # diagIn = 27;
  # ppi = diagPx / diagIn;          => 163.178308895
  #
  # source: https://www.calculatorsoup.com/calculators/technology/ppi-calculator.php
  services.xserver.dpi = 163;
}
