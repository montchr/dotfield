# Search engine options reference: <https://searchfox.org/mozilla-central/rev/669329e284f8e8e2bb28090617192ca9b4ef3380/toolkit/components/search/SearchEngine.jsm#1138-1177>
{
  lib,
  lib',
  pkgs,
}:
let
  inherit (lib'.firefox) engine engine';
in
{
  # Required -- disabled by default to prevent unintentional data loss.
  force = true;

  default = lib.mkDefault "Kagi";
  privateDefault = "DuckDuckGo";

  engines = {
    "Kagi" = engine "https://kagi.com/search?q={searchTerms}";
    "Letterboxd" = engine' "lb" "https://letterboxd.com/search/{searchTerms}";
    "Marginalia" =
      engine' "m" "https://search.marginalia.nu/search?query={searchTerms}&profile=default&js=default";
    "npm" = engine' "npm" "https://www.npmjs.com/search?q={searchTerms}";
    # NOTE: Requires setting HTTP method to GET in SearXNG Preferences -> Privacy
    "priv.au" = engine' "s" "https://priv.au/search?q={searchTerms}";
    "Power Thesaurus" = engine' "thes" "https://www.powerthesaurus.org/{searchTerms}/synonyms";
    "TVDB" = engine' "tvdb" "https://thetvdb.com/search?query={searchTerms}";
    "WordPress Code Reference" = engine' "wp" "https://developer.wordpress.org/?s={searchTerms}";

    ## === Nix Reference ===

    "home-manager options" =
      engine' "hm" "https://home-manager-options.extranix.com/?query={searchTerms}";
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
        "@nixopts"
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
      urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];
      iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
      # TODO: why?
      updateInterval = 24 * 60 * 60 * 1000; # every day
      definedAliases = [ "@nwiki" ];
    };
    "Noogle" = engine' "nixlib" "https://noogle.dev/?term=%22{searchTerms}%22";
  };
}
