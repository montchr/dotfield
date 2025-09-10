{
  aspects.workstation.home = {
    programs.qutebrowser = {
      enable = true;
      enableDefaultBindings = true;
      # Load settings configured in the GUI.
      loadAutoconfig = true;
      searchEngines = {
        # TODO: share with firefox
        "DEFAULT" = "https://kagi.com/search?q={}";
        "lb" = "https://letterboxd.com/search/{}";
        "m" = "https://search.marginalia.nu/search?query={}&profile=default&js=default";
        "npm" = "https://www.npmjs.com/search?q={}";
        # NOTE: Requires setting HTTP method to GET in SearXNG Preferences -> Privacy
        "s" = "https://priv.au/search?q={}";
        "thes" = "https://www.powerthesaurus.org/{}/synonyms";
        "tvdb" = "https://thetvdb.com/search?query={}";
        "wp" = "https://developer.wordpress.org/?s={}";
      };
    };
  };
}
