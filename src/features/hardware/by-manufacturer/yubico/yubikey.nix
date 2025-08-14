{ lib, ... }:
{
  dotfield.features.workstation.nixos =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        hardware.gpgSmartcards.enable = true;
        services.udev.packages = [ pkgs.yubikey-personalization ];

        # using this requires use of `disable-ccid` in scdaemon.conf, and
        # even still, it may be the cause of breaking pinentry for me
        # 2024-05-05?
        # <https://github.com/colemickens/nixcfg/blob/9baee703ddbba93250ef2feb74b25fb3625cbc12/mixins/gpg-agent.nix>
        services.pcscd.enable = false;
      }
      (lib.mkIf config.services.kanata.enable {
        services.kanata.keyboards."default".extraDefCfg = ''
          linux-dev-names-exclude (
            "Yubico YubiKey OTP+FIDO+CCID"
          )
        '';
      })
    ];

  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.age-plugin-yubikey
        pkgs.yubikey-personalization
        # FIXME: broken on nixos-unstable 2024-05-05
        # pkgs.yubikey-manager
      ];
      services.gpg-agent.enableScDaemon = true;
    };
}
