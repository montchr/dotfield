{
  config,
  lib,
  pkgs,
  suites,
  ...
}: {
  imports = suites.minimal;
}
