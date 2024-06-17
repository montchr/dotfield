_: {
  # Will be overridden by the bootstrapIso module.
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
  };

  users.users.nixos = {
    password = "nixos";
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  home-manager.users.nixos = hmArgs: {
    imports = [
      ../../home/mixins/graphical.nix
      ../../home/mixins/developer.nix
    ];

    home.stateVersion = "22.11";
  };

  system.stateVersion = "22.11";
}
