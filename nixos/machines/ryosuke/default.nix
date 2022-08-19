{
  config,
  hmUsers,
  lib,
  pkgs,
  profiles,
  suites,
  inputs,
  peers,
  primaryUser,
  ...
}: let
  inherit (config.networking) hostName;
in {
  imports =
    [
      ./hardware-configuration.nix
    ]
    ++ (with suites;
      tangible
      ++ workstation
      ++ opsbox)
    ++ (with profiles; [
      boot.systemd-boot
      hardware.amd
      login.gdm
      # login.greetd
      remote-builder
      # virtualisation.vm-variant
    ]);

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.initrd.supportedFilesystems = ["ext4" "btrfs"];
  boot.supportedFilesystems = ["ext4" "btrfs"];

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-bd80ec2d-c0ae-4132-b0c2-4cd10bf59a78".device = "/dev/disk/by-uuid/bd80ec2d-c0ae-4132-b0c2-4cd10bf59a78";
  boot.initrd.luks.devices."luks-bd80ec2d-c0ae-4132-b0c2-4cd10bf59a78".keyFile = "/crypto_keyfile.bin";

  virtualisation.vmVariant = {
    virtualisation.graphics = false;
  };

  system.stateVersion = "22.05";

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  ### === networking ===========================================================

  networking = lib.mkIf (!config.nixos-vm.enable) (
    let
      #host = peers.hosts.${hostName};
      #net = peers.networks.${host.network};
      #interface = "eth0";
    in {
      networkmanager.enable = true;
      wireless.enable = true; # Enables wireless support via wpa_supplicant.
      useDHCP = true;
      usePredictableInterfaceNames = false;

      firewall = {
        enable = true;
        # allowedTCPPorts = [80 443];
      };
    }
  );

  ### === users ================================================================

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

  users.mutableUsers = false;
  users.users.root.initialHashedPassword = "$6$0H8NbKLF5uDD.rvG$S46H2N8W0wKiRRCZOlE5QXBxZCd9CEU0rRi5kZdLMfvcMaYGMC9OEojjAW9i/3c6vRktxQnSUwv4xIZlOOjlB/";
  users.users.cdom = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$XTIAlt33Lwfe309d$Zbthi9TYLdnxxKawGXzzX2QawlWkssJkYwP5NsxVb4430IRWz6TEtQfGdp5A9If5kRgj3BS2aacRsFxprfyKy.";
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups =
      [
        "wheel"
        "video"
        "audio"
        "seadome"
        "secrets"
      ]
      ++ (lib.optional config.networking.networkmanager.enable "networkmanager")
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.virtualisation.docker.enable "docker")
      ++ (lib.optional config.virtualisation.podman.enable "podman")
      ++ (lib.optional config.virtualisation.libvirtd.enable "libvirtd")
      ++ (lib.optional config.virtualisation.virtualbox.host.enable "vboxusers");
    shell = pkgs.fish;
  };

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "cdom";

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  home-manager.users = {
    cdom = hmArgs: {
      imports =
        (with hmArgs.suites; workstation ++ opsbox)
        ++ [hmUsers.cdom];
    };
  };

  programs.htop.enable = true;
}
