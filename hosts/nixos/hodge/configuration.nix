# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Required for broadcom driver support.
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  # Thunderbolt support.
  nixpkgs.config.packageOverrides = pkgs: {
    linux = pkgs.linuxPackages.override {
      extraConfig = ''
        THUNDERBOLT m
      '';
    };
  };

  nix = {
  #  package = pkgs.nixUnstable;
    useSandbox = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  networking = {
    hostName = "hodge"; # Define your hostname.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.ens9.useDHCP = true; # Ethernet
    interfaces.wlp3s0.useDHCP = true; # WiFi

    wireless = {
      enable = true;
      interfaces = [ "wlp3s0" ];
      userControlled.enable = true;
    };
  };

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    # Large font size for use on HiDPI screen
    font = "ter-i32b";
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
#    useXkbConfig = true;
  };

  boot.cleanTmpDir = true;

  # boot.extraModprobeConfig = ''
  #  options libata.force=noncq
  #  options resume=/dev/sda3
  #  options hid_apple fnmode=2
  # '';

  services.xserver = {
    enable = true;
    layout = "us";
#    xkbVariant = "mac";
#    xkbOptions = "terminate:ctrl_alt_bksp, ctrl:nocaps";
    displayManager = {
      lightdm.enable = true;
      defaultSession = "plasma";
      autoLogin = {
        enable = true;
        user = "xtallos";
      };
    };
    desktopManager.plasma5.enable = true;
    libinput.enable = true;
    videoDrivers = [ "nvidia" ];

#    synaptics.additionalOptions = ''
#      Option "VertScrollDelta" "-100"
#      Option "HorizScrollDelta" "-100"
#    '';
#    synaptics.enable = true;
#    synaptics.tapButtons = true;
#    synaptics.fingersMap = [ 0 0 0 ];
#    synaptics.buttonsMap = [ 1 3 2 ];
#    synaptics.twoFingerScroll = true;
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

  powerManagement.enable = true;
  services.upower.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.xtallos = {
    isNormalUser = true;
    hashedPassword = "$6$yq7jJybfGyx19QqK$mr1dfKu1fChKkYDUZvQnlcKCmAYywIvWZXw3uT9EjQ/Xi85SGqkPDcsrrQ.7WEYM6InqDPqGZrTGfvoFpuONi1";
    extraGroups = [ "wheel" "network-manager" ]; # Enable ‘sudo’ for the user.
  };
  home-manager.users.xtallos = { pkgs, ... }: {
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
    fd
    kate
    konsole
    glxinfo
    ripgrep
    vim
    wget
    firefox
    linuxPackages.broadcom_sta
    xclip
    tldr
    bat
    fzf
    git
    # wirelesstools
    # networkmanagerapplet
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh = {
    enable = true;
    hostKeys = [
      {
        path = "/etc/ssh/id_ed25519";
        type = "ed25519";
      }
    ];
  };

#  programs.ssh.startAgent = true;

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

