# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

# Search engine options reference: <https://searchfox.org/mozilla-central/rev/669329e284f8e8e2bb28090617192ca9b4ef3380/toolkit/components/search/SearchEngine.jsm#1138-1177>

{
  lib,
  lib',
  pkgs,
}:
let
  inherit (lib) mapAttrsToList nameValuePair;
  inherit (lib'.firefox)
    engine
    engine'
    engineWithAliases
    withSearchAliasPrefix
    ;

  withNixIcon =
    attrs:
    attrs
    // {
      icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
    };

  mkNixSearchTemplate = aliases: template: withNixIcon (engineWithAliases aliases template);

  mkNixpkgsSearchTemplate =
    type: aliases: params:
    (withNixIcon {
      definedAliases = aliases;
      urls = [
        {
          template = "https://search.nixos.org/${type}";
          params = mapAttrsToList nameValuePair params;
        }
      ];
    });

  mkNixpkgsSearchTemplate' = type: aliases: mkNixpkgsSearchTemplate type aliases { };
in
{
  # Required -- disabled by default to prevent unintentional data loss.
  force = true;

  default = lib.mkDefault "kagi";
  privateDefault = "ddg";

  engines = {
    kagi = engine "https://kagi.com/search?q={searchTerms}";
    letterboxd = engineWithAliases [
      "letterboxd"
      "lb"
    ] "https://letterboxd.com/search/{searchTerms}";
    marginalia = engineWithAliases [
      "marginalia"
      "m"
    ] "https://marginalia-search.com/search?query={searchTerms}";
    powerthesaurus = engine' "thes" "https://www.powerthesaurus.org/{searchTerms}/synonyms";
    tvdb = engine' "tvdb" "https://thetvdb.com/search?query={searchTerms}";
    wp = engine' "wp" "https://developer.wordpress.org/?s={searchTerms}";

    ## Nix

    hm = mkNixpkgsSearchTemplate "options" [ "hm" ] { source = "home_manager"; };
    nixos-wiki = mkNixSearchTemplate [
      "nixos"
      "nwiki"
    ] "https://wiki.nixos.org/w/index.php?search={searchTerms}";
    noogle = mkNixSearchTemplate [ "nixlib" "noogle" ] "https://noogle.dev/q/?term={searchTerms}";
    nixpkgs = mkNixpkgsSearchTemplate' "packages" [
      "nixpkgs"
      "npkgs"
      "pkgs"
    ];
    nixos-options = mkNixpkgsSearchTemplate' "options" [
      "nixopts"
      "nopts"
      "nixos-options"
    ];
    nixpkgs-pr = mkNixSearchTemplate [
      "nixpr"
      "nixpkgs-pr"
    ] "https://nixpk.gs/pr-tracker.html?pr={searchTerms}";
  };
}
