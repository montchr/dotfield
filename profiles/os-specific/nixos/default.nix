{
  self,
  config,
  lib,
  pkgs,
  ...
}: {
  nix = {
    autoOptimiseStore = true;
    nixPath = ["nixos-config=${../../../lib/compat/nixos}"];
    optimise.automatic = true;
    systemFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
  };

  shellAliases = {
   # Fix `nixos-option` for flake compatibility
   nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  };

  # FIXME: only for tangible machines
  networking.wireless = {
    # Defines environment variables for wireless network passkeys.
    environmentFile = config.age.secrets."wireless.env".path;
    networks.bortHole.psk = "@PSK_BORTHOLE@";
  };

  environment.systemPackages = with pkgs; [
    dosfstools
    gptfdisk
    iputils
    usbutils
    utillinux
  ];

  i18n.defaultLocale = "en_US.UTF-8";

  # For rage encryption, all hosts need a ssh key pair
  services.openssh = {
    enable = true;
    openFirewall = lib.mkDefault false;
  };

  # Automatically kill processes when running low on available memory.
  # https://github.com/rfjakob/earlyoom
  services.earlyoom.enable = true;
}
