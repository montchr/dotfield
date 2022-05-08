{
  config,
  pkgs,
  lib,
  hmUsers,
  suites,
  profiles,
  ...
}:

let
  secretsDir = ../../../secrets;
in

{
  imports =
    (with suites; basic)
    ++ (with profiles; [users.seadoom])
    ++ [./hardware-configuration.nix];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.devices = ["/dev/sda"];

  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;
  networking.firewall.enable = true;

  environment.variables.DOTFIELD_DIR = "/etc/dotfield";

  services.openssh.enable = true;
  services.openssh.openFirewall = true;
  services.openssh.permitRootLogin = "no";

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  security.doas.enable = true;
  security.doas.wheelNeedsPassword = false;

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = (import "${secretsDir}/authorized-keys.nix");
  users.users.root.initialHashedPassword = "$6$HimRTytkPSPaFqxi$jqOi8wZhhDunVpHlAtBvOol6J.Gk3l0PeEdiLVkI.f3JoDwT4OixyOcetfz.X87.0m3PqJjU9OgllBTY919bx1";
  users.users.seadoom = {
    hashedPassword = "$6$OlgpB7UeQh/f7hi7$5Kq/fDAEXS01Qv1XynDaBr/SPjNicBPDBhXIsiWsdj76QdehPp3oJA5w8uueOz63UXajdCMw6tQFMvFn6d19Z1";
    openssh.authorizedKeys.keys = (import "${secretsDir}/authorized-keys.nix");
  };
  home-manager.users.seadoom = {
    config,
    suites,
    ...
  }: {
    imports = [hmUsers.seadoom];

    lib.dotfield.userPath = "/etc/dotfield";
  };

  system.stateVersion = "21.11";
}
