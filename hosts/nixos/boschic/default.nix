{ config, hmUsers, lib, pkgs, profiles, ... }:

{
  imports =
    (with profiles; [
      fonts.common
      fonts.pragmatapro
    ])
    ++ [./hardware-configuration.nix];

  networking.hostName = "boschic";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nixpkgs.config.allowUnfree = true;
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "America/New_York";

  ## --- networking ---

  networking.useDHCP = false;
  networking.interfaces.enp8s0.useDHCP = true;
  networking.interfaces.wlp6s0.useDHCP = true;

  ## --- desktop ---

  hardware.nvidia.modesetting.enable = true;
  # hardware.opengl.enable = true;

  environment.variables = {
    # TODO: still necessary with firefox-wayland package?
    MOZ_ENABLE_WAYLAND = "1";
  };

  # TODO: still necessary even with wayland?
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.gnome.chrome-gnome-shell.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = true;
  services.xserver.displayManager.gdm.nvidiaWayland = true;
  services.xserver.displayManager.gdm.autoSuspend = false;
  services.xserver.desktopManager.gnome.enable = true;
  programs.xwayland.enable = true;


  # Configure keymap in X11
  services.xserver.layout = "us";
  # FIXME: doesn't work
  services.xserver.xkbOptions = "caps:ctrl_modifier";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  services.pipewire.enable = true;
  services.pipewire.alsa.enable = true;
  services.pipewire.alsa.support32Bit = true;
  services.pipewire.pulse.enable = true;
  hardware.pulseaudio.enable = false;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  xdg.portal.enable = true;
  xdg.portal.gtkUsePortal = true;

  # programs._1password-gui.enable = true;
  # programs._1password.enable = true;


  ### === users ================================================================

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$HshRirQmQu.nxnwE$6eUWz9pN3T9F4KZVBpz7KfvZhLAFRGRHkm1YFsIqpQUSHBw8Lfh6G6PBLbHp9/XUxiIz0MZQaxRqQvHMIn/hW0";
  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$ARl/PHPTN16/aGSi$oCAM1JsVDKWuhogrV/9TwNOxN2.tFaN3SlpG6tB0wvKNksuzFp8CHd2Z6AQSPq35DsLfJprw4DdYy/CzEweON.";
    hashedPassword = "$6$ARl/PHPTN16/aGSi$oCAM1JsVDKWuhogrV/9TwNOxN2.tFaN3SlpG6tB0wvKNksuzFp8CHd2Z6AQSPq35DsLfJprw4DdYy/CzEweON.";
    extraGroups = [ "wheel" "video" "network-manager" "seadome" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  home-manager.users.seadoom = hmArgs: {
    imports = [hmUsers.seadoom];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    neovim
    wget
    firefox-wayland
    ripgrep
    fd
    git
    curl
    tealdeer
    bat
    _1password
    _1password-gui
    plex-media-player
    vscode
  ];

  hardware.video.hidpi.enable = true;
  services.xserver.dpi = 192;

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  nix.trustedUsers = [ "root" "@wheel" "@seadome" ];


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
