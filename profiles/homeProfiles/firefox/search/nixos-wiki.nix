{
  urls = [{template = "https://nixos.wiki/index.php?search={searchTerms}";}];
  iconUpdateURL = "https://nixos.wiki/favicon.png";
  updateInterval = 24 * 60 * 60 * 1000; # every day
  definedAliases = ["@nw"];
}
