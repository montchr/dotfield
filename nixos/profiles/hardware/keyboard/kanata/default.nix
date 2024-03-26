{ config, ... }:
let
  inherit (builtins) readFile;
in
{
  assertions = [
    {
      assertion = !(config.services.keyd.enable);
      message = "keyd conflicts with kanata";
    }
    {
      assertion = !(config.services.kmonad.enable);
      message = "kmonad conflicts with keyd";
    }
  ];

  # Required: allow kernel-level input event interception
  # <https://wiki.archlinux.org/title/Input_remap_utilities>
  dotfield.guardian.extraGroups = [
    "input"
    "uinput"
  ];

  services.kanata = {
    enable = true;

    # FIXME: remove
    # extraArgs = [
    #   "--log-level"
    #   "debug"
    # ];

    # NOTE: The device path is intentionally omitted.
    keyboards."default" = {
      # config =
      #   ''

      #   ''
      #   + (readFile ./kanata-arsenik.kbd);
      # extraDefCfg = '''';
    };
  };
}
