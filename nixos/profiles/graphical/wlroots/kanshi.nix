# <https://sr.ht/~emersion/kanshi/>
#
# > kanshi allows you to define output profiles that are automatically
# > enabled and disabled on hotplug. For instance, this can be used to
# > turn a laptop's internal screen off when docked.
# >
# > This is a Wayland equivalent for tools like autorandr. kanshi can be
# > used on Wayland compositors supporting the wlr-output-management
# > protocol.
{ pkgs, ... }:
{
  systemd.user.services.kanshi = {
    description = "kanshi daemon";
    environment = {
      WAYLAND_DISPLAY = "wayland-1";
      DISPLAY = ":0";
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = ''${pkgs.kanshi}/bin/kanshi -c kanshi_config_file'';
    };
  };
}
