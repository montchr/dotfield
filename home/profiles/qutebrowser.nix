_:
{
  programs.qutebrowser = {
    enable = true;
    loadAutoconfig = true;
    searchEngines = {
      # TODO: share with firefox
      "DEFAULT" = "https://kagi.com/search?q={}";
    };
  };
}
