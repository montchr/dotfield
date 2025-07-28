{
  dotfield.modules.networkmanager.nixos = {
    networking.networkmanager.enable = true;
  };

  dotfield.modules.admin-user.nixos = {
    dotfield.guardian.extraGroups = [ "networkmanager" ];
  };
}
