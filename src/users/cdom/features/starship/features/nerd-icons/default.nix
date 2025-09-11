{
  # Graphical because this depends on a font that supports Nerd Icons.
  # FIXME: because both this base layer and the full configuration are
  # set wholesale from TOML, there's no way to prevent conflicting
  # definitions, so both need to be merged in only one definition.
  # users.cdom.aspects.graphical.home
  users.cdom.aspects.noop.home = {
    programs.starship.settings =
      builtins.readFile ./config/dot-config/starship.toml |> builtins.fromTOML;
  };
}
