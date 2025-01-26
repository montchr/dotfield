{
  services.espanso.matches.urls.matches = [
    {
      replace = "https://github.com/";
      trigger = ";@gh";
    }
    {
      replace = "https://kleinweb.atlassian.net/browse/KWG-";
      trigger = ";@jira";
    }
    {
      regex = ";gh\\((?P<repo>.*)\\)";
      replace = "[{{repo}}](https://github.com/{{repo}})";
    }
    {
      regex = ";jira<(?P<id>\\d+)>";
      replace = "https://kleinweb.atlassian.net/browse/KWG-{{id}}";
    }
  ];
}
