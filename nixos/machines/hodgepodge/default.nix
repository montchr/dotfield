{
  config,
  lib,
  pkgs,
  primaryUser,
  profiles,
  suites,
  ...
}: {
  imports =
    (with suites; tangible ++ workstation)
    ++ (with (profiles.shared); [
      hidpi
      login.gdm
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

  services.printing.enable = true;
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
    imports = with hmArgs.roles; workstation;

    home.packages = with pkgs; [
      teams
    ];

    programs.kitty.settings."font_size" = "11.0";
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
