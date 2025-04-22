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

  default = lib.mkDefault "kagi";
  privateDefault = "ddg";

  engines = {
    kagi = engine "https://kagi.com/search?q={searchTerms}";
    letterboxd = engine' "lb" "https://letterboxd.com/search/{searchTerms}";
    marginalia = engine' "m" "https://search.marginalia.nu/search?query={searchTerms}&profile=default&js=default";
    powerthesaurus = engine' "thes" "https://www.powerthesaurus.org/{searchTerms}/synonyms";
    tvdb = engine' "tvdb" "https://thetvdb.com/search?query={searchTerms}";
    wp = engine' "wp" "https://developer.wordpress.org/?s={searchTerms}";

    ## === Nix Reference ===

    hm-options = engine' "hm" "https://home-manager-options.extranix.com/?query={searchTerms}";
    nixpkgs = {
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
    nixos-options = {
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
    nixos-wiki = {
      urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];
      definedAliases = [ "@nwiki" ];
    };
    nix = engine' "nixlib" "https://noogle.dev/?term=%22{searchTerms}%22";
  };
}
