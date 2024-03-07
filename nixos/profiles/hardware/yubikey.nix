# FIXME: since recent update, not working properly, with _apparent_ errors running `ykman list` despite correct output
#        - it definitely hasn't been working with gnupg user service
#        - maybe nixos gnupg module needs enabling? tho iirc that can cause difficulty by hogging $GPG_TTY before graphical environment starts...
#
#        <https://github.com/NixOS/nixpkgs/issues/280826>
#        -- does not seem to help.
#        <https://github.com/drduh/YubiKey-Guide/issues/376>
#
{
  config,
  lib,
  pkgs,
  ...
}:
{
  # Required when using `gnome3`. The package will be included automatically
  # when using the GNOME desktop environment, but if not, the package must be
  # added manually.
  services.dbus.packages = [ pkgs.gcr ];

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
