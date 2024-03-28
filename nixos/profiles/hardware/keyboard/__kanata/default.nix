{ lib, config, ... }:
let
  inherit (builtins) readFile;
  cfg = config.services.kanata;
in
{
  config =
    lib.mkIf
      (
        config.dotfield.hardware.keyboard.remapping.enable
        && config.dotfield.hardware.keyboard.remapping.provider == "kanata"
      )
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
        # <https://github.com/jtroo/kanata/blob/v1.5.0/docs/avoid-sudo-linux.md>
        # <https://wiki.archlinux.org/title/Input_remap_utilities>
        dotfield.guardian.extraGroups = [
          "input"
          "uinput"
        ];

        # The NixOS module does not automatically make the package available.
        environment.systemPackages = [ cfg.package ];

        services.kanata = {
          enable = true;

          keyboards."default" = {
            # config =
            #   ''

            #   ''
            #   + (readFile ./kanata-arsenik.kbd);
            # extraDefCfg = '''';
          };
        };
      };
}
