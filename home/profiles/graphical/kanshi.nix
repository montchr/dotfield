# <https://sr.ht/~emersion/kanshi/>
#
# > kanshi allows you to define output profiles that are automatically
# > enabled and disabled on hotplug. For instance, this can be used to
# > turn a laptop's internal screen off when docked.
# >
# > This is a Wayland equivalent for tools like autorandr. kanshi can be
# > used on Wayland compositors supporting the wlr-output-management
# > protocol.
{
  config,
  flake,
  pkgs,
  ...
}:
let
  cfg = config.services.kanshi;
in
{
  services.kanshi = {
    enable = true;
    # package = flake.perSystem.inputs'.nixpkgs-wayland.packages.kanshi;
    profiles = { };
  };

  home.packages = [ cfg.package ];
}
