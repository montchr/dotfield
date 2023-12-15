{config, ...}: {
  imports = [
    ./hardware-configuration.nix
    ./profiles/sops.nix
    ./users/seadoom.nix
  ];

  time.timeZone = "America/New_York";

  boot.loader.efi.canTouchEfiVariables = true;
  services.printing.enable = true;
  hardware.facetimehd.enable = true;

  networking.usePredictableInterfaceNames = false;
  networking.firewall.enable = true;

  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 2880 1800;      => 3396.23320754
  # diagIn = 15;
  # ppi = diagPx / diagIn;        => 226.415547169
  services.xserver.dpi = 226;

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "seadoom";
  users.mutableUsers = false;

  system.stateVersion = "21.11"; # Did you read the comment?
}
