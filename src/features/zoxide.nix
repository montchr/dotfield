{
  aspects.core.home =
    { config, ... }:
    {
      programs.zoxide.enable = true;
      home.sessionVariables."_ZO_DATA_DIR" = config.xdg.dataHome;
    };
}
