{
  config,
  lib,
  pkgs,
  ...
}: {
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableWebService = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;

  environment.systemPackages = with pkgs; [
    vagrant
    virtualbox
  ];

  virtualisation.vmVariant = {
    virtualisation.graphics = false;
  };
}
