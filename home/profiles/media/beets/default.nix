{
  flake,
  lib,
  config,
  ...
}:
let
  inherit (flake.inputs) wrapper-manager;
  inherit (config) xdg;

  musicDir = config.xdg.userDirs.music;

  cfg = config.programs.beets;
in

{
  # wrappers."beet" = {
  #   basePackage = cfg.package;
  #   prependFlags = ["--config" ]
  # };

  programs.beets = {
    enable = true;
    settings = {
      library = "${musicDir}/library.db";
      directory = "${musicDir}/data";
      log = "${xdg.stateHome}/beets/beet.log";

      plugins = [
        "chroma"
        "discogs"
        "edit"
        "embedart"
        "export"
        "fetchart"
        "fromfilename"
        "fuzzy"
        "info"
        "mbsync"
        "playlist"
        "replaygain"
        "scrub"
        "smartplaylist"
      ];

      import = {
        bell = true;
        copy = false;
        duplicate_action = "ask";
        incremental = true;
        move = true;
        resume = true;
        write = true;
      };

      paths = {
        default = "%first{$albumartist}/$album%aunique{}%if{$multidisc,$disc-}%if{$track,$track - }$title";
      };

      per_disc_numbering = true;

      match = {
        # <https://beets.readthedocs.io/en/stable/reference/config.html#max-rec>
        max_rec.missing_tracks = "low";
        medium_rec_thresh = 0.25;
        preferred = {
          countries = [
            "XW"
            "US"
            "UK|GB"
            "DE"
          ];
          media = [
            "Digital Media|File"
            "CD"
          ];
        };
        rec_gap_thresh = 0.25;
        strong_rec_thresh = 0.15;

        ignored = "missing_tracks unmatched_tracks";
        ignored_media = [
          "Data CD"
          "DVD"
          "DVD-Video"
          "Blu-ray"
          "HD-DVD"
          "VCD"
          "SVCD"
          "UMD"
          "VHS"
        ];
      };

      musicbrainz = {
        extra_tags = [
          "year"
          "catalognum"
          "country"
          "media"
          "label"
        ];
      };

      fetchart.auto = true;
      fetchart.sources = [
        "filesystem"
        "coverart"
        "discogs"
        "amazon"
        "albumart"
      ];

      # Use the original release date instead of the edition release date.
      original_date = true;

      replaygain = {
        auto = true;
        backend = "ffmpeg";
        threads = 2;
        # NOTE: Must run `beet write` after import!
        parallel_on_import = true;
      };

      edit.itemfields = [
        "track"
        "title"
        "artist"
        "album"
        "year"
        "month"
        "day"
      ];
      edit.albumfields = [
        "album"
        "albumartist"
        "albumdisambig"
        "year"
        "month"
        "day"
      ];

      ui.color = true;

      item_fields.multidisc = "1 if disctotal > 1 else 0";
      # TODO: keep or remove?
      # item_fields.artist_differs = "1 if albumartist != artist else 0";

      languages = [ "en" ];

      ignore_hidden = true;
      asciify_paths = true;
      max_filename_length = 255;
    };
  };
}
