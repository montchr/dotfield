{ lib, pkgs, ... }:
{
  qt.enable = true;

  # FIXME: agnostic defaults
  qt.platformTheme = lib.mkDefault "gnome";
  qt.style.package = lib.mkDefault pkgs.adwaita-qt;
  qt.style.name = lib.mkDefault "adwaita";
}
