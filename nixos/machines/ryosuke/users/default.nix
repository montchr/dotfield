{...}: {
  imports = [
    ./cdom.nix
    ./root.nix
  ];
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;
}
