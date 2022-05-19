{
  config,
  lib,
  inputs,
  pkgs,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  imports =
    (with suites; tangible ++ workstation)
    ++ (with profiles; [
      users.xtallos
    ])
    ++ [
      ./broadcom.nix
      ./hardware-configuration.nix
    ];

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$OZwXf5o.yZUoehTw$29dItBi5.fjd2GZToR6sP2F.xEUn/5TBKZlZ6CHNazEAWz6roIevLATX82QV9rCxlZ21sL9zsW..LmTFhC/dx.";
  users.users.xtallos = {
    hashedPassword = "$6$yq7jJybfGyx19QqK$mr1dfKu1fChKkYDUZvQnlcKCmAYywIvWZXw3uT9EjQ/Xi85SGqkPDcsrrQ.7WEYM6InqDPqGZrTGfvoFpuONi1";
    extraGroups = ["network-manager"];
  };

  home-manager.users.xtallos = hmArgs@{
    pkgs,
    ...
  }: {
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
