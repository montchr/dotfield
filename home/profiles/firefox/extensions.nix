{isBukuEnabled}: {
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
    (lib.mkIf isBukuEnabled bukubrow)
    copy-selection-as-markdown
    darkreader
    display-_anchors
    firefox-color
    (lib.mkIf config.programs.browserpass.enable browserpass)
    mailvelope
    multi-account-containers
    octolinker
    # FIXME: only use this if not using tridactyl (e.g. guest users on htpc)
    # old-reddit-redirect
    org-capture
    pinboard
    # FIXME: needs configuration, probably
    # promnesia
    # protondb-for-steam
    react-devtools
    reddit-enhancement-suite
    reduxdevtools
    # TODO: set default preferences for this and others? is that possible?
    refined-github
    return-youtube-dislikes
    sidebery
    single-file
    sourcegraph
    tab-session-manager
    temporary-containers
    tridactyl
    ublock-origin

    ##: Themes {{

    # TODO: add this to upstream repo
    # arctic-nord-theme

    theme-nord-polar-night

    ##: }}
  ];
}
