{
  dotfield.baseline.home = {
    programs.starship.settings =
      builtins.readFile ./config/nerd-icons/dot-config/starship.toml |> builtins.fromTOML;
  };
}
