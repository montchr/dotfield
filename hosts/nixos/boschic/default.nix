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
    (with suites; tangible ++ workstation)
    ++ (with profiles; [
      boot.refind
      hardware.amd
      remote-builder
      workstations.flatpak
      virtualisation.libvirtd
      virtualisation.vm-variant

      # Why use the proprietary drivers? While things *generally* work, I
      # encountered frustrating issues:
      #
      # - Video playback performance was unacceptable.
      #
      # - Upon connecting/disconnecting displays while logged in, the session
      # would lock. Although both displays would be available in the login
      # manager, upon logging back in, neither display would work, with both of
      # them displaying black. I could still SSH in, but could not figure out
      # how to recover without rebooting.
      nvidia

      # FIXME: only apply this to the proper output -- will not currently allow
      # configuring multiple outputs.
      # hardware.displays.LG-27GL850-B
    ])
    ++ [
      ./hardware-configuration.nix
      # ./virtualisation.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "auto";
  # FIXME: does this interfere with rEFInd? if not this, then i blame Windows.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 15;
  boot.initrd.supportedFilesystems = ["ext4" "btrfs"];
  boot.supportedFilesystems = ["ext4" "btrfs"];

  system.stateVersion = "21.11";

  ### === networking ===========================================================

  networking = lib.mkIf (!config.nixos-vm.enable) (
    let
      host = peers.hosts.${hostName};
      net = peers.networks.${host.network};
      interface = "eth0";
    in {
      useDHCP = false;
      usePredictableInterfaceNames = false;
      # interfaces.wlp6s0.useDHCP = true;
      firewall.enable = false;

      defaultGateway = {
        inherit interface;
        inherit (net.ipv4) address;
      };

      interfaces.${interface} = {
        ipv4.addresses = [
          {
            inherit (host.ipv4) address;
            inherit (net.ipv4) prefixLength;
          }
        ];
      };
    }
  );

  ### === users ================================================================

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$HshRirQmQu.nxnwE$6eUWz9pN3T9F4KZVBpz7KfvZhLAFRGRHkm1YFsIqpQUSHBw8Lfh6G6PBLbHp9/XUxiIz0MZQaxRqQvHMIn/hW0";
  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$ARl/PHPTN16/aGSi$oCAM1JsVDKWuhogrV/9TwNOxN2.tFaN3SlpG6tB0wvKNksuzFp8CHd2Z6AQSPq35DsLfJprw4DdYy/CzEweON.";
    hashedPassword = "$6$ARl/PHPTN16/aGSi$oCAM1JsVDKWuhogrV/9TwNOxN2.tFaN3SlpG6tB0wvKNksuzFp8CHd2Z6AQSPq35DsLfJprw4DdYy/CzEweON.";
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups =
      [
        "wheel"
        "video"
        "audio"
        "networkmanager"
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
  users.users.zortflower = {
    uid = 1001;
    isNormalUser = true;
    hashedPassword = "$6$vKXBAWMIBgK2lqZM$px7zUItEknMtXUriGTHwS6S2zmmyvZTVfk6vD4mcLIQNS4nBalfLkT4spjeoEI1ock.0Dk9.qKR8Wlze3GDJ40";
    extraGroups = [
      "video"
      "networkmanager"
    ];
  };

  home-manager.users = {
    seadoom = hmArgs: {
      imports =
        (with hmArgs.suites; workstation)
        ++ (with hmArgs.profiles; [virtualisation.vagrant])
        ++ [hmUsers.seadoom];
    };
    zortflower = hmArgs: {
      imports =
        (with hmArgs.suites; graphical)
        ++ [hmUsers.nixos];
    };
  };

  programs.htop.enable = true;
  programs.steam.enable = true;
}
