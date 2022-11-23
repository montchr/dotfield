_: {
  # Will be overridden by the bootstrapIso module.
  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};

  users.users.nixos = {
    password = "nixos";
    description = "default";
    isNormalUser = true;
    extraGroups = ["wheel"];
  };

  home-manager.users.nixos = hmArgs: {
    imports = with hmArgs.roles; graphical ++ developer;
    home.stateVersion = "22.05";
  };

  system.stateVersion = "22.05";
}
