## Sources:
# <https://github.com/montchr/stormobservatory/blob/8c26fbfebdd437c3d50446002766b9b9410a85a2/src/beets/config.yaml>
# <https://github.com/Ramblurr/nixcfg/blob/5140a2049ac6dfae528ca60c4ffccbff553d638d/hosts/mali/beets.nix>
# <https://github.com/foo-dogsquared/nixos-config/blob/e64d10f2aac3031c07aaef8b4dc481f055cec072/configs/home-manager/foo-dogsquared/modules/setups/music.nix>
{
  flake,
  pkgs,
  config,
  ...
}:
let
  inherit (config) xdg;
  musicDir = config.xdg.userDirs.music;
  separator = "; ";
in

{
  nixpkgs.overlays = [ (import ../../../../overlays/beets.nix) ];

  programs.beets = {
    enable = true;
    package = pkgs.beets-unstable.override {
      pluginOverrides = {
        filetote = {
          enable = true;
          propagatedBuildInputs = [ flake.perSystem.packages.beets-filetote ];
        };
      };
    };
    settings = {
      library = "${musicDir}/library.db";
      directory = "${musicDir}/data";
      log = "${xdg.stateHome}/beets/beet.log";

      plugins = [
        "albumtypes"
        # "badfiles" # slow -- only use as needed
        "chroma"
        "discogs"
        "edit"
        "embedart"
        "export"
        "fetchart"
        "filetote"
        "fromfilename"
        "fuzzy"
        "info"
        "inline"
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
        languages = "en";
        move = true;
        resume = true;
        write = true;
      };

      # Use the original release date instead of the edition release date.
      original_date = true;

      per_disc_numbering = true;
      item_fields = {
        multidisc = "1 if disctotal > 1 else 0";
        artist_differs = "1 if albumartist != artist else 0";
      };

      paths = {
        default = "%first{$albumartist}/$album%aunique{}/%if{$multidisc,$disc-}$track - $title";
      };

      badfiles = {
        check_on_import = true;
      };

      filetote = {
        extensions = [
          ".cue"
          ".nfo"
          ".pdf"
        ];
        filenames = "cover.jpg cover.png";
        pairing = {
          enabled = true;
          pairing_only = true;
          extensions = [ ".lrc" ];
        };
      };

      ui.color = true;

      ignore_hidden = true;
      asciify_paths = true;
      max_filename_length = 255;

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
        # Use these extra metadata to narrow API queries.
        extra_tags = [
          "year"
          "catalognum"
          "country"
          "media"
          "label"
        ];
        # NOTE: separator is "; " -- not configurable
        genres = true;
        # Related metadata sources stored as library fields.  Existing data
        # overwritten upon re-import.
        external_ids = {
          bandcamp = true;
          discogs = true;
          spotify = true;
        };
      };

      # https://beets.readthedocs.io/en/stable/plugins/discogs.html#configuration
      discogs = {
        inherit separator;
        # "Techno" vs. only "Electronic"
        append_style_genre = true;
        # Consider matches with same weight as the MusicBrainz source.  MB data
        # is generally less complete and more prone to mistakes.  There is
        # currently no way to disable MusicBrainz entirely.
        source_weight = 0.0;
        # Example:
        # <https://www.discogs.com/Handel-Sutherland-Kirkby-Kwella-Nelson-Watkinson-Bowman-Rolfe-Johnson-Elliott-Partridge-Thomas-The-A/release/2026070>
        # true => "Athalia, Act I, Scene I: Sinfonia"
        # false => "Sinfonia"
        index_tracks = true;
      };

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
    };
  };
}
