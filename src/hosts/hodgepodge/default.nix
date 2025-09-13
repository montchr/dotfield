##: MacBookPro11,3 Core i7 2.3GHz 15in. (Late 2013) (Dual-Graphics)
# <https://everymac.com/systems/apple/macbook_pro/specs/macbook-pro-core-i7-2.3-15-dual-graphics-late-2013-retina-display-specs.html>
{ config, lib, ... }:
{
  hosts.nixos.hodgepodge = {
    system = "x86_64-linux";

    aspects = with config.aspects; [
      workstation
      hardware__apple__macbookpro-11-3
      desktop-sessions__gnome
    ];

    configuration = {
      time.timeZone = "America/New_York";

      boot.loader.efi.canTouchEfiVariables = true;

      services.tailscale.enable = true;

      networking.usePredictableInterfaceNames = false;
      networking.firewall.enable = true;

      users.mutableUsers = false;

      sops.defaultSopsFile = ./secrets/secrets.yaml;

      system.stateVersion = "21.11"; # Did you read the comment?
    };

    baseline.home = {
      ### Hardware oddities specific to this machine:
      dconf.settings = {
        # This machine's trackpad physical button is physically broken.
        # Without tap-to-click, it would not possible to use the trackpad.
        "org/gnome/desktop/peripherals/touchpad".tap-to-click = lib.mkForce true;

        # The keyboard is also starting to go... but a super-thorough deep clean
        # might help...
        "org/gnome/desktop/a11y/applications".screen-keyboard-enabled = true;
      };
    };
  };
}
