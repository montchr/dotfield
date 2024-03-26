# <https://github.com/kmonad/kmonad/blob/master/doc/installation.md#configurationnix>
{ ... }:
{
  # Required: allow kernel-level input event interception
  # <https://wiki.archlinux.org/title/Input_remap_utilities>
  dotfield.guardian.extraGroups = [
    "input"
    "uinput"
  ];

  services.kmonad = {
    enable = true;

    # FIXME: remove
    extraArgs = [
      "--log-level"
      "debug"
    ];

    # NOTE: The device path is intentionally omitted.
    keyboards."default" = {
      defcfg = {
        enable = true;
        compose.key = "ralt";
        compose.delay = 5;
        fallthrough = true;
      };
    };
  };
}
