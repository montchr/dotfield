{ lib, ... }:
{
  qt.enable = true;
  qt.platformTheme.name = lib.mkDefault "gtk3";
}
