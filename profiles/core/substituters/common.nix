{
  imports = [
    ./dotfield.nix
    ./nix-community.nix
  ];
  nix.settings = {
    substituters = ["https://cache.nixos.org/"];
    trusted-substituters = ["https://cache.nixos.org/"];
    trusted-public-keys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="];
  };
}
