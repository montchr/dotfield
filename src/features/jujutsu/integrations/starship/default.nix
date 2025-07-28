{ lib, ... }:
{
  dotfield.modules.development.home =
    { config, ... }:
    lib.mkIf config.programs.starship.enable {
      programs.starship.settings =
        (builtins.readFile ./config/dot-config/starship.toml) |> builtins.fromTOML;
    };
}
