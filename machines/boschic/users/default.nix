{ ... }:
{
  imports = [
    ./seadoom.nix
    ./zortflower.nix
  ];
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "seadoom";
  users.mutableUsers = false;
}
