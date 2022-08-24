{...}: {
  imports = [
    ./seadoom.nix
  ];
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "seadoom";
  users.mutableUsers = false;

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;
}
