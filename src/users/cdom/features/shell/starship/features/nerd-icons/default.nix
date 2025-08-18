{
  dotfield.baseline.home = {
    programs.starship.settings =
      builtins.readFile ./config/dot-config/starship.toml |> builtins.fromTOML;
  };
}
