{
  users.cdom.aspects.core.home = {
    programs.starship.enable = true;
    programs.starship.settings =
      builtins.readFile ./config/dot-config/starship.toml |> builtins.fromTOML;
  };
}
