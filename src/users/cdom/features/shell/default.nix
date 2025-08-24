{
  dotfield.baseline.home = {
    programs.dircolors.enable = true;
    programs.carapace.enable = true;
    programs.info.enable = true;

    programs.readline = {
      enable = true;
      variables = {
        # Disable the internal pager.
        page-completions = false;
      };
    };
  };
}
