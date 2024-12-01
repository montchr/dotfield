{
  programs.beets.settings.match = {
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
}
