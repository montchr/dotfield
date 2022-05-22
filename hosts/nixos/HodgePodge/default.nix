{
  config,
  lib,
  inputs,
  pkgs,
  suites,
  profiles,
  hmUsers,
  primaryUser,
  ...
}: {
  imports =
    (with suites; tangible ++ workstation)
    ++ [
      ./broadcom.nix
      ./hardware-configuration.nix
    ];

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$OZwXf5o.yZUoehTw$29dItBi5.fjd2GZToR6sP2F.xEUn/5TBKZlZ6CHNazEAWz6roIevLATX82QV9rCxlZ21sL9zsW..LmTFhC/dx.";
  users.users.seadoom = {
    extraGroups = ["wheel" "network-manager"];
    hashedPassword = "$6$j9n2NQ.HRZ3VGIZh$NT3YkL3cDUy/ZQ5Oi6mDIEdfxQ2opgUVD7HIZTqRDcqsJqQiukmkZNIcxSVGQ.fgP38utHDBGcl4V20iodB9M.";
    isNormalUser = true;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  };

  home-manager.users.seadoom = hmArgs: {
    imports =
      [hmUsers.xtallos]
      ++ (with hmArgs.suites; workstation)
      ++ (with hmArgs.profiles; [mail]);

    home.packages = with pkgs; [
      ddate
      _1password
      kitty
    ];
  };

  environment.systemPackages = with pkgs; [
    firefox
    glxinfo
    kate
    konsole
    ripgrep
    tldr
    vim
    wget
    xsel
    # wirelesstools
    # networkmanagerapplet
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = ["btrfs"];

  boot.cleanTmpDir = true;

  networking.interfaces = {
    enp0s20u1.useDHCP = true;
    wlp3s0.useDHCP = true;
    # TODO: these are probably invalid
    ens9.useDHCP = true;
    ens0.useDHCP = true;
  };
  networking.wireless = {
    enable = true;
    interfaces = ["wlp3s0"];
    userControlled.enable = true;
  };
  networking.firewall.enable = false;

  console = {
    # Large font size for use on HiDPI screen
    font = "ter-i32b";
    packages = with pkgs; [terminus_font];
    keyMap = "us";
  };

  # videoDrivers = [ "nvidia" ];

  hardware.video.hidpi.enable = true;

  # See https://en.wikipedia.org/wiki/Retina_display
  services.xserver.dpi = 221;
}
