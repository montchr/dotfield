{
  config,
  pkgs,
  lib,
  hmUsers,
  suites,
  profiles,
  ...
}: let
  secretsDir = ../../../secrets;
in {
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

  environment.variables.DOTFIELD_DIR = "/etc/nixos";

  services.openssh.enable = true;
  # TODO: should this be locked down further?
  services.openssh.openFirewall = true;
  services.openssh.permitRootLogin = "no";

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  security.doas.enable = true;
  security.doas.wheelNeedsPassword = false;

  users.users.root.hashedPassword = "$6$4SlbVdLk7nOBSUSM$jfNjHJG7rKBhdv8KbOLD0bMiXfZ2Tsh8yXQYY0MYMBAU4vejixQWBL5gEq/A219uUIKbYgSpxAFyySqnfdwaw1";

  users.mutableUsers = false;
  users.users.seadoom = {
    hashedPassword = "$6$j9n2NQ.HRZ3VGIZh$NT3YkL3cDUy/ZQ5Oi6mDIEdfxQ2opgUVD7HIZTqRDcqsJqQiukmkZNIcxSVGQ.fgP38utHDBGcl4V20iodB9M.";
    openssh.authorizedKeys.keys = import "${secretsDir}/authorized-keys.nix";
  };
  home-manager.users.seadoom = {...}: {
    imports = [hmUsers.seadoom];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    fd
    fish
    ripgrep
    tealdeer
    vim
    wget
  ];

  system.stateVersion = "21.11";
}
