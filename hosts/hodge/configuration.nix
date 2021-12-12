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
      networks = {
        bortHole = {
          priority = 0;
          # FIXME: needs a value
          # psk = "";
        };
      };
      interfaces = [ "wlp3s0" ];
      userControlled.enable = true;
    };
  };

  # Set your time zone.
  time.timeZone = "America/New_York";


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    # Large font size for use on HiDPI screen
    font = "ter-i32b";
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
  };

  services.xserver = {
    enable = true;
    layout = "us";
    displayManager = {
      lightdm.enable = true;
      defaultSession = "plasma5";
      autoLogin = {
        enable = true;
        user = "cdom";
      };
    };
    desktopManager.plasma5.enable = true;
    libinput.enable = true;
    # FIXME: ran into an SSL error, possibly related to Let's Encrypt issues from 2021-09-30
    # might need a flake input update
    # videoDrivers = [ "nvidia" ];
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.cdom = {
  #  isNormalUser = true;
  #  extraGroups = [ "wheel" "network-manager" ]; # Enable ‘sudo’ for the user.
  # };

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
    hostKeys = {
      path = "/etc/ssh/id_ed25519";
      type = "ed25519";
    };
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
  system.stateVersion = "21.05"; # Did you read the comment?

}

