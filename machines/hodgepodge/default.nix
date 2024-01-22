##: MacBookPro11,3 Core i7 2.3GHz 15in. (Late 2013) (Dual-Graphics)
# <https://everymac.com/systems/apple/macbook_pro/specs/macbook-pro-core-i7-2.3-15-dual-graphics-late-2013-retina-display-specs.html>
{lib, ...}: {
  imports = [
    ./hardware-configuration.nix
    ./profiles/sops.nix
    ./users/seadoom.nix
    ./graphics.nix
  ];

  time.timeZone = "America/New_York";

  boot.loader.efi.canTouchEfiVariables = true;
  services.printing.enable = true;

  ## Hardware oddities specific to this machine
  # FIXME: quality is terrible -- linux-specific or just because this thing is old?
  hardware.facetimehd.enable = true;
  home-manager.sharedModules = lib.singleton {
    dconf.settings."org/gnome/desktop/peripherals/touchpad" = {
      # NOTE: This machine's trackpad physical button is physically broken.
      # Without tap-to-click, it would not possible to use the trackpad.
      tap-to-click = lib.mkForce true;
    };
  };

  networking.usePredictableInterfaceNames = false;
  networking.firewall.enable = true;

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "seadoom";
  users.mutableUsers = false;

  system.stateVersion = "21.11"; # Did you read the comment?
}
