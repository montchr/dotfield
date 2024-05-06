{ lib, pkgs, ... }:
{
  # using this requires use of `disable-ccid` in scdaemon.conf,
  # and even still, it may be the cause of breaking pinentry for me 2024-05-05?
  # <https://github.com/colemickens/nixcfg/blob/9baee703ddbba93250ef2feb74b25fb3625cbc12/mixins/gpg-agent.nix>
  services.pcscd.enable = false;

  hardware.gpgSmartcards.enable = true;

  services.udev.packages = [ pkgs.yubikey-personalization ];

  home-manager.sharedModules = lib.singleton {
    home.packages = [
      pkgs.yubikey-personalization
      # FIXME: broken on nixos-unstable 2024-05-05
      # pkgs.yubikey-manager
    ];
  };
}
