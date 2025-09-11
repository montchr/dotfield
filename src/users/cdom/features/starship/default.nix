{ inputs, ... }:
let
  inherit (builtins) fromTOML readFile;
in
{
  users.cdom.aspects.core.home = {
    programs.starship.enable = true;
    programs.starship.settings = inputs.dmerge.merge (
      readFile ./features/nerd-icons/config/dot-config/starship.toml |> fromTOML
    ) (readFile ./config/dot-config/starship.toml |> fromTOML);
  };
}
