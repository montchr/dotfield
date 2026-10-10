{
  aspects.noctalia.home =
    { config, lib, ... }:
    let
      inherit (lib) mkDefault;
      inherit (config.xdg.userDirs) videos;
    in
    {
      programs.noctalia.settings = {
        plugins.enabled = [ "noctalia/screen_recorder" ];

        plugin_settings."noctalia/screen_recorder".directory = mkDefault "${videos}/Screencasts";

        recorder.type = "noctalia/screen_recorder:recorder";
      };
    };
}
