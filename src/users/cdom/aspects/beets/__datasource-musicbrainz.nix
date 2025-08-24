{
  programs.beets.settings.musicbrainz = {
    enabled = false;

    # Use these extra metadata to narrow API queries.
    extra_tags = [
      "year"
      "catalognum"
      "country"
      "media"
      "label"
    ];

    # NOTE: separator is "; " -- not configurable
    # genres = true;

    # Related metadata sources stored as library fields.  Existing data
    # overwritten upon re-import.
    external_ids = {
      bandcamp = true;
      discogs = true;
      spotify = true;
    };
  };
}
