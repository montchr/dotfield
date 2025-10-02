# Currently required for Asahi monitor support via USB-C.  Asahi does not yet
# support DP-Alt display output.  DP-Alt output is required for true HDMI or
# DP output via one of this machine's two USB-C ports and zero HDMI/DP ports.
#
# For details on update procedure, follow the messages shown during the
# initial rebuild, and/or see <https://wiki.nixos.org/wiki/Displaylink>.
{
  aspects.hardware__displaylink.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.displaylink ];
      services.xserver.videoDrivers = [
        "displaylink"
        "modesetting"
      ];
      systemd.services.dlm.wantedBy = [ "multi-user.target" ];
    };
}
