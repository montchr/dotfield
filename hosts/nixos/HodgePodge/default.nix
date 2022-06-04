{
  config,
  lib,
  pkgs,
  hmUsers,
  primaryUser,
  profiles,
  suites,
  ...
}: {
  imports =
    (with suites; tangible ++ workstation)
    ++ (with profiles; [
      hidpi
      office
      # nvidia
    ])
    ++ [
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.interfaces.enp0s20u1.useDHCP = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.xserver.libinput.enable = true;

  hardware.facetimehd.enable = true;

  users.mutableUsers = false;
  users.users.seadoom = {
    isNormalUser = true;
    uid = 1000;
    initialHashedPassword = "$6$uCcWeO4zLGaoG/QC$c/YTM4ASYrg4IBr8MD0.XziuvBFR4r/4HwpxS/5/gFbZLk2p9QQ69NrR.fGC58iYut54HSbvbVMBLSkiJ5But0";
    hashedPassword = "$6$uCcWeO4zLGaoG/QC$c/YTM4ASYrg4IBr8MD0.XziuvBFR4r/4HwpxS/5/gFbZLk2p9QQ69NrR.fGC58iYut54HSbvbVMBLSkiJ5But0";
    extraGroups = [
      "wheel"
      "seadome"
      "secrets"
      "video"
      "networkmanager"
    ];
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    shell = pkgs.zsh;
  };

  home-manager.users.seadoom = hmArgs: {
    imports =
      [hmUsers.seadoom]
      ++ (with hmArgs.suites; workstation);
  };

  programs.htop.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    _1password-gui
    _1password
  ];

  networking.firewall.enable = false;

  system.stateVersion = "21.11"; # Did you read the comment?

  nixpkgs.config.allowUnfree = true;
}
