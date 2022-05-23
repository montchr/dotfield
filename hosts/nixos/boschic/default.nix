{ config, hmUsers, lib, pkgs, profiles, suites, ... }:

{
  imports =
    (with suites; tangible ++ workstation)
    ++ (with profiles; [
      hidpi
      nvidia
    ])
    ++ [./hardware-configuration.nix];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;

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

  home-manager.users.seadoom = hmArgs: {
    imports = [hmUsers.seadoom];
  };

  programs.htop.enable = true;
  programs.steam.enable = true;
}
