{
  # https://beets.readthedocs.io/en/stable/plugins/discogs.html#configuration
  programs.beets.settings.discogs = {
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
}
