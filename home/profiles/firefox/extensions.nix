{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs) firefox-addons;
in {
  # TODO: add zotero connector addon
  programs.firefox.extensions = with firefox-addons; [
    onepassword-password-manager
    a11ycss
    add-custom-search-engine
    bitwarden
    copy-selection-as-markdown
    darkreader
    display-_anchors
    firefox-color
    (lib.mkIf config.programs.browserpass.enable browserpass)
    kagi-search
    mailvelope
    multi-account-containers
    octolinker
    old-reddit-redirect
    org-capture
    raindropio
    react-devtools
    reddit-enhancement-suite
    reduxdevtools
    refined-github
    sidebery
    single-file
    tab-session-manager
    temporary-containers
    tridactyl
  ];
}
