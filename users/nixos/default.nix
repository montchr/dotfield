{ features, ... }:
{
  users.users.nixos = {
    password = "nixos";
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  home-manager.users.nixos = _: { imports = [ ../../../home/mixins/graphical.nix ]; };
}
