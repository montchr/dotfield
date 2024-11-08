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
in

{
  programs.beets = {
    enable = true;
    package = pkgs.beets.override {
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
        move = true;
        resume = true;
        write = true;
      };

      # Use the original release date instead of the edition release date.
      original_date = true;

      languages = [ "en" ];
      per_disc_numbering = true;
      item_fields = {
        multidisc = "1 if disctotal > 1 else 0";
        artist_differs = "1 if albumartist != artist else 0";
      };

      paths = {
        # XXX: discogs plugin often fails with `%first{}`
        # <https://github.com/beetbox/beets/issues/5473>
        # default = "%first{$albumartist}/$album%aunique{}/%if{$multidisc,$disc-}%if{$track,$track - }$title";
        default = "$albumartist/$album%aunique{}/%if{$multidisc,$disc-}%if{$track,$track - }$title";
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
        extra_tags = [
          "year"
          "catalognum"
          "country"
          "media"
          "label"
        ];
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
        # NOTE: Must run `beet write` after import!
        parallel_on_import = true;
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
