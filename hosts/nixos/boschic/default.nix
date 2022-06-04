{
  config,
  hmUsers,
  lib,
  pkgs,
  profiles,
  suites,
  ...
}: {
  imports =
    (with suites; tangible ++ workstation)
    ++ (with profiles; [
      boot.refind
      hidpi
      nvidia
    ])
    ++ [./hardware-configuration.nix];

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "auto";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 15;

  system.stateVersion = "21.11";

  ## --- networking ---

  networking.useDHCP = false;
  networking.interfaces.enp8s0.useDHCP = true;
  networking.interfaces.wlp6s0.useDHCP = true;
  networking.firewall.enable = false;

  ## --- desktop ---

  # FIXME: if needed, be careful, as this may need extra config for nvidia
  # hardware.opengl.enable = true;

  ### === users ================================================================

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$HshRirQmQu.nxnwE$6eUWz9pN3T9F4KZVBpz7KfvZhLAFRGRHkm1YFsIqpQUSHBw8Lfh6G6PBLbHp9/XUxiIz0MZQaxRqQvHMIn/hW0";
  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$ARl/PHPTN16/aGSi$oCAM1JsVDKWuhogrV/9TwNOxN2.tFaN3SlpG6tB0wvKNksuzFp8CHd2Z6AQSPq35DsLfJprw4DdYy/CzEweON.";
    hashedPassword = "$6$ARl/PHPTN16/aGSi$oCAM1JsVDKWuhogrV/9TwNOxN2.tFaN3SlpG6tB0wvKNksuzFp8CHd2Z6AQSPq35DsLfJprw4DdYy/CzEweON.";
    extraGroups = [
      "wheel"
      "video"
      "networkmanager"
      "seadome"
      "secrets"
    ];
    shell = pkgs.zsh;
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
