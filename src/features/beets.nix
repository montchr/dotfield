{ lib, ... }:
{
  dotfield.aspects.workstation.home =
    { config, ... }:
    let
      cfg = config.programs.beets;
    in
    lib.mkIf cfg.enable {
      services.mpd.musicDirectory = cfg.settings.directory;
    };
}
