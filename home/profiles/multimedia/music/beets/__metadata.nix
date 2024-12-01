{
  programs.beets.settings = {
    scrub = {
      auto = false;
    };

    # Use the original release date instead of the edition release date.
    original_date = true;

    per_disc_numbering = true;
    item_fields = {
      multidisc = "1 if disctotal > 1 else 0";
      artist_differs = "1 if albumartist != artist else 0";
    };
  };
}
