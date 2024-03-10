{ lib, ... }:
{
  qt.enable = true;
  qt.platformTheme = lib.mkDefault "gtk3";
}
