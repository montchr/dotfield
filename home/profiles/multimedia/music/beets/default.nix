## Sources:
# <https://github.com/montchr/stormobservatory/blob/8c26fbfebdd437c3d50446002766b9b9410a85a2/src/beets/config.yaml>
# <https://github.com/Ramblurr/nixcfg/blob/5140a2049ac6dfae528ca60c4ffccbff553d638d/hosts/mali/beets.nix>
# <https://github.com/foo-dogsquared/nixos-config/blob/e64d10f2aac3031c07aaef8b4dc481f055cec072/configs/home-manager/foo-dogsquared/modules/setups/music.nix>
{
  lib,
  flake,
  pkgs,
  config,
  ...
}:
let
  inherit (config) xdg;

  cfg = config.programs.beets;

  musicDir = config.xdg.userDirs.music;
  separator = "; ";
in

{
  imports = [
    ./__files.nix
    ./__imports.nix
    ./__lookup.nix
    ./__datasource-bandcamp.nix
    ./__datasource-discogs.nix
    ./__datasource-musicbrainz.nix
    ./__metadata.nix
    ./__metadata-genre.nix
  ];

  #  home.packages = [ flake.perSystem.packages.beetcamp ];

  programs.beets = {
    enable = true;
    package =
      flake.perSystem.inputs'.nixpkgs-for-beets-not-failing-build.legacyPackages.beets.override
        {
          pluginOverrides = {
            # FIXME: broken with beets 2.1.0
            # beetcamp = {
            #   enable = true;
            #   propagatedBuildInputs = [ flake.perSystem.packages.beetcamp ];
            # };
            # FIXME: broken when overriding beets from older
            # nixpkgs...? thinks beets package is not available
            # filetote = {
            #   enable = true;
            #   propagatedBuildInputs = [ flake.perSystem.packages.beets-filetote ];
            # };
            # summarize = {
            #   enable = true;
            #   propagatedBuildInputs = [ flake.perSystem.packages.beet-summarize ];
            # };
          };
        };
    settings = {
      library = "${musicDir}/library.db";
      directory = "${musicDir}/data";
      log = "${xdg.stateHome}/beets/beet.log";

      plugins = [
        "albumtypes"

        # "badfiles" # slow -- only use as needed

        # FIXME: breaks
        # "bandcamp" # aka "beetcamp"

        "chroma"
        "discogs"
        "edit"
        "embedart"
        "export"
        "fetchart"
        "filetote"
        "fish"
        "fromfilename"
        "fuzzy"
        "importfeeds"
        "info"
        "inline"
        "lastgenre"
        "mbsync"
        "missing"
        "playlist"
        "replaygain"
        "scrub"
        "smartplaylist"
        "summarize"
        "the"
        "thumbnails"
        "unimported"
      ];

      ui.color = true;

      fetchart = {
        auto = true;
        sources = [
          "filesystem"
          "coverart"
          "discogs"
          "amazon"
          "albumart"
        ];
      };

      replaygain = {
        auto = true;
        backend = "ffmpeg";
        threads = 2;
        # NOTE: When true, must run `beet write` after import!
        parallel_on_import = false;
      };

      edit = {
        itemfields = [
          "track"
          "title"
          "artist"
          "album"
          "year"
          "month"
          "day"
        ];
        albumfields = [
          "album"
          "albumartist"
          "albumdisambig"
          "year"
          "month"
          "day"
        ];
      };

      unimported = {
        ignore_extensions = "cue jpg jpeg lrc md nfo pdf png txt webp";
      };
    };
  };
}
