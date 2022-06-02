{
  self,
  config,
  lib,
  pkgs,
  ...
}: {
  nix = {
    autoOptimiseStore = true;
    nixPath = ["nixos-config=${../../lib/compat/nixos}"];
    optimise.automatic = true;
    systemFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
  };

  environment.shellAliases = {
    # Fix `nixos-option` for flake compatibility
    nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  };

  environment.systemPackages = with pkgs; [
    dosfstools
    efibootmgr
    gptfdisk
    iputils
    usbutils
    utillinux
  ];

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  users.groups.secrets.members = ["root" "seadoom" "xtallos"];

  services.openssh = {
    # For rage encryption, all hosts need a ssh key pair
    enable = lib.mkForce true;

    openFirewall = true;
    passwordAuthentication = false;
    permitRootLogin = "prohibit-password";
  };

  # Automatically kill processes when running low on available memory.
  # https://github.com/rfjakob/earlyoom
  services.earlyoom.enable = true;
}
