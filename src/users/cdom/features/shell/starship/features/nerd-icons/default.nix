{ lib, ... }:
let
  inherit (builtins) fromTOML mapAttrs readFile;

in
{

  dotfield.baseline.home = {
    programs.starship.settings =
      readFile ./config/dot-config/starship.toml |> fromTOML |> mapAttrs (_: v: lib.mkDefault v);
  };
}
