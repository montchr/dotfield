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

  # kanata: exclude yubikeys
  services.kanata.keyboards."default".extraDefCfg = ''
    linux-dev-names-exclude (
      "Yubico YubiKey OTP+FIDO+CCID"
    )
  '';

  # TODO: possible to use a wildcard in place of the product id to catch all
  #       from Yubico vendor?  manpage says nothing on this one way or the
  #       other.
  # keyd: exclude yubikeys
  services.keyd.keyboards.default.ids = [
    "-1050:0407" # Yubico YubiKey OTP+FIDO+CCID
  ];
}
