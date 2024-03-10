{ lib, pkgs, ... }:
{
  services.pcscd.enable = true;

  hardware.gpgSmartcards.enable = true;

  environment.systemPackages = [
    # FIXME: <https://github.com/NixOS/nixpkgs/issues/280826>
    pkgs.pcscliteWithPolkit.out
  ];

  services.udev.packages = [ pkgs.yubikey-personalization ];

  home-manager.sharedModules = lib.singleton {
    home.packages = [
      pkgs.yubikey-personalization
      pkgs.yubikey-manager
    ];
  };
}
