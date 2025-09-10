{
  aspects.hardware__yubico__yubikey = {
    nixos =
      { lib, pkgs, ... }:
      {
        # using this requires use of `disable-ccid` in scdaemon.conf,
        # and even still, it may be the cause of breaking pinentry for me 2024-05-05?
        # <https://github.com/colemickens/nixcfg/blob/9baee703ddbba93250ef2feb74b25fb3625cbc12/mixins/gpg-agent.nix>
        services.pcscd.enable = false;

        hardware.gpgSmartcards.enable = true;

        services.udev.packages = [ pkgs.yubikey-personalization ];

        environment.systemPackages = [
          pkgs.age-plugin-yubikey
          pkgs.yubikey-personalization
          # FIXME: broken on nixos-unstable 2024-05-05
          # pkgs.yubikey-manager
        ];
      };

    home =
      { pkgs, ... }:
      {
        home.packages = [
          # XXX: build failure for dependency python3.12-pyscard-2.1.1
          #  pkgs.yubikey-manager
        ];
        services.gpg-agent.enableScDaemon = true;
      };
  };
}
