# TODO: provide link to docs on supported metadata (somewhere in mozilla source code prob)
{ lib, pkgs }:
let
  engine = template: { urls = lib.singleton { inherit template; }; };
  withAlias = s: attrs: attrs // { definedAliases = lib.singleton s; };
  engine' = alias: template: withAlias "@${alias}" (engine template);
in
{
  # Required -- disabled by default to prevent unintentional data loss.
  force = true;

  default = lib.mkDefault "Kagi";

  engines = {
    "Kagi" = engine "https://kagi.com/search?q={searchTerms}";
    "Marginalia" = engine' "m" "https://search.marginalia.nu/search?query={searchTerms}&profile=default&js=default";
    "npm" = engine' "npm" "https://www.npmjs.com/search?q={searchTerms}";
    # NOTE: Requires setting HTTP method to GET in SearXNG Preferences -> Privacy
    "priv.au" = engine' "s" "https://priv.au/search?q={searchTerms}";
    "Power Thesaurus" = engine' "thes" "https://www.powerthesaurus.org/{searchTerms}/synonyms";

    ## === Nix Reference ===

    "home-manager options" = engine' "hm" "https://mipmip.github.io/home-manager-option-search/?query={searchTerms}";
    "Nix Packages" = {
      definedAliases = [
        "@nixpkgs"
        "@pkgs"
      ];

      urls = [
        {
          template = "https://search.nixos.org/packages";
          params = [
            {
              name = "type";
              value = "packages";
            }
            {
              name = "query";
              value = "{searchTerms}";
            }
          ];
        }
      ];
      icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
    };
    "NixOS Options" = {
      definedAliases = [
        "@nixos"
        "@opts"
      ];

      urls = [
        {
          # Example: https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=gnome
          template = "https://search.nixos.org/options";
          params = [
            # {
            #   name = "type";
            #   # Yes, even for Options search (though I don't think it's required)
            #   value = "packages";
            # }
            {
              name = "channel";
              value = "unstable";
            }
            {
              name = "query";
              value = "{searchTerms}";
            }
            {
              name = "sort";
              value = "relevance";
            }
          ];
        }
      ];
      icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
    };
    "NixOS Wiki" = {
      urls = [ { template = "https://nixos.wiki/index.php?search={searchTerms}"; } ];
      iconUpdateURL = "https://nixos.wiki/favicon.png";
      # TODO: why?
      updateInterval = 24 * 60 * 60 * 1000; # every day
      definedAliases = [ "@nwiki" ];
    };
    "Noogle" = engine' "nixlib" "https://noogle.dev/?term=%22{searchTerms}%22";
  };
}
