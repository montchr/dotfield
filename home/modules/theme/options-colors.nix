# FIXME: consider restoring the simpler name-only approach to avoid the hard
#        dependency on a color library even if the module is not enabled.
{
  config,
  lib,
  colorSchemes,
  colorSchemeOptions,
  ...
}: let
  inherit (lib) types;
  colorSchemeType = types.submodule {options = colorSchemeOptions;};
  mkColorSchemeOption = kind:
    lib.mkOption {
      default = colorSchemes."default-${kind}";
      type = colorSchemeType;
    };
in {
  options = {
    active = lib.mkOption {
      type = with types; nullOr colorSchemeType;
      default = config.dark;
      description = ''
        Currently-active color scheme, defaulting to the value of the dark theme.
      '';
    };
    dark = mkColorSchemeOption "dark";
    light = mkColorSchemeOption "light";
  };
}
