{
  config,
  pkgs,
  hmUsers,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = ["btrfs"];

  # Required for broadcom driver support.
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;

  # Thunderbolt support.
  # TODO: don't enable this until you know exactly what it does
  # nixpkgs.config.packageOverrides = pkgs: {
  #   linux = pkgs.linuxPackages.override {
  #     extraConfig = ''
  #       THUNDERBOLT m
  #     '';
  #   };
  # };

  nix = {
    #  package = pkgs.nixUnstable;
    useSandbox = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  networking = {
    hostName = "HodgePodge";

    interfaces.enp0s20u1.useDHCP = true; # Ethernet
    interfaces.wlp3s0.useDHCP = true; # WiFi

    # TODO: these are probably invalid
    interfaces.ens9.useDHCP = true; # Ethernet
    interfaces.ens0.useDHCP = true; # Ethernet

    wireless = {
      enable = true;
      interfaces = ["wlp3s0"];
      userControlled.enable = true;
    };
  };

  time.timeZone = "America/New_York";

  boot.cleanTmpDir = true;
  # videoDrivers = [ "nvidia" ];

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    # Large font size for use on HiDPI screen
    font = "ter-i32b";
    packages = with pkgs; [terminus_font];
    keyMap = "us";
    #    useXkbConfig = true;
  };

  hardware.video.hidpi.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    # See https://en.wikipedia.org/wiki/Retina_display
    dpi = 221;

    desktopManager.plasma5.enable = true;
    displayManager.sddm.enable = true;

    # xkbVariant = "mac";
    # xkbOptions = "terminate:ctrl_alt_bksp, ctrl:nocaps";
  };

  fonts.fontDir.enable = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fonts = with pkgs; [
    corefonts
    inconsolata
    liberation_ttf
    dejavu_fonts
    bakoma_ttf
    gentium
    ubuntu_font_family
    terminus_font
  ];

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.xtallos = {
    isNormalUser = true;
    hashedPassword = "$6$yq7jJybfGyx19QqK$mr1dfKu1fChKkYDUZvQnlcKCmAYywIvWZXw3uT9EjQ/Xi85SGqkPDcsrrQ.7WEYM6InqDPqGZrTGfvoFpuONi1";
    extraGroups = ["wheel" "network-manager"]; # Enable ‘sudo’ for the user.
  };

  home-manager.users.xtallos = {pkgs, ...}: {
    imports = [hmUsers.xtallos];
    home.packages = with pkgs; [
      ddate
      _1password
      kitty
    ];
    programs.fish.enable = true;
  };

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  users.users.root.hashedPassword = "$6$OZwXf5o.yZUoehTw$29dItBi5.fjd2GZToR6sP2F.xEUn/5TBKZlZ6CHNazEAWz6roIevLATX82QV9rCxlZ21sL9zsW..LmTFhC/dx.";

  users.mutableUsers = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    bat
    fd
    firefox
    fzf
    git
    glxinfo
    kate
    konsole
    linuxPackages.broadcom_sta
    ripgrep
    tldr
    vim
    wget
    xsel
    # wirelesstools
    # networkmanagerapplet
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  services.openssh = {
    enable = true;
    # hostKeys = [
    #   {
    #     path = "/etc/ssh/id_ed25519";
    #     type = "ed25519";
    #   }
    # ];
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
