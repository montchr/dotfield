{
  config,
  lib,
  pkgs,
  ...
}: {
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableWebService = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  dotfield.guardian.user.extraGroups = ["vboxusers"];
}
