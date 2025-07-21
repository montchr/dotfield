{
  flake.modules.nixos.workstation = {
    networking.networkmanager.enable = true;
  };

  flake.modules.nixos.admin-user = {
    dotfield.guardian.extraGroups = [ "networkmanager" ];
  };
}
