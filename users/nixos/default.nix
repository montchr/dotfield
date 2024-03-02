{features, ...}: {
  users.users.nixos = {
    password = "nixos";
    description = "default";
    isNormalUser = true;
    extraGroups = ["wheel"];
  };

  home-manager.users.nixos = hmArgs: {imports = with hmArgs.features; graphical;};
}
