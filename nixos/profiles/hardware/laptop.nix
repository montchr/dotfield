{
  flake,
  pkgs,
  lib,
  ...
}:
{
  services.upower.enable = true;
  networking.networkmanager.wifi.powersave = true;

  environment.systemPackages = [ pkgs.poweralertd ];

  # NOTE: Requires that the $laptop variable is set!  This should be
  # added in a laptop-specific module!
  environment.etc."sway/config".text = lib.mkAfter ''
    # Clamshell mode
    # <https://github.com/swaywm/sway/wiki#user-content-clamshell-mode>
    bindswitch --reload --locked lid:on output $laptop disable
    bindswitch --reload --locked lid:off output $laptop enable
  '';

  home-manager.sharedModules = [ (flake.self + "/home/profiles/hardware/laptop.nix") ];
}
