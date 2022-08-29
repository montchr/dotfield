moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs) firefox-wayland runCommandNoCC writeText;
  inherit (lib) concatStrings mapAttrsToList;
  inherit (pkgs.sources) firefox-lepton-ui;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (pkgs.nur.repos.rycee) firefox-addons;

  themeFonts = config.theme.font;

  isBukuEnabled = config.programs.buku.enable && config.programs.buku.enableBrowserIntegration;

  cfg = config.programs.firefox;
  homeProfilePath = ".mozilla/firefox/${cfg.profiles.home.path}";
  workProfilePath = ".mozilla/firefox/${cfg.profiles.work.path}";

  hostName = moduleArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");


  leptonPath = firefox-lepton-ui.src;
  leptonSettings = import ./lepton-settings.nix;
  # These CSS files must be `@import`ed in order to preserve relative URIs
  # expected for icons.
  leptonChrome = ''
    @import url("${leptonPath}/css/leptonChrome.css");
  '';
  leptonContent = ''
    @import url("${leptonPath}/css/leptonContent.css");
  '';

  makeProfileSettings = profile:
    writeText "user.js" (concatStrings
      (mapAttrsToList (name: value: ''
          user_pref("${name}", ${builtins.toJSON value});
        '')
        cfg.profiles.${profile}.settings));

  makeProfileSettingsFile = profile:
    runCommandNoCC "firefox-${profile}-settings" {} ''
      cat '${leptonPath}/user.js' '${makeProfileSettings profile}' > $out
    '';

  userChrome = leptonChrome;
  userContent =
    leptonContent
    + (builtins.readFile ./userContent.css);

  # TODO: consider removing. if you want a firefox without telemetry, use
  # librewolf. otherwise let mozilla do its thing.
  disableTelemetry = {
    "browser.newtabpage.activity-stream.feeds.telemetry" = false;
    "browser.newtabpage.activity-stream.telemetry" = false;
    "browser.ping-centre.telemetry" = false;
    "toolkit.telemetry.archive.enabled" = false;
    "toolkit.telemetry.bhrPing.enabled" = false;
    "toolkit.telemetry.enabled" = false;
    "toolkit.telemetry.firstShutdownPing.enabled" = false;
    "toolkit.telemetry.hybridContent.enabled" = false;
    "toolkit.telemetry.newProfilePing.enabled" = false;
    "toolkit.telemetry.reportingpolicy.firstRun" = false;
    "toolkit.telemetry.shutdownPingSender.enabled" = false;
    "toolkit.telemetry.unified" = false;
    "toolkit.telemetry.updatePing.enabled" = false;
  };

  privacySettings = {
    "network.dns.disablePrefetch" = true;
    "privacy.donottrackheader.enabled" = true;
    "privacy.donottrackheader.value" = 1;
    "privacy.trackingprotection.enabled" = true;
    "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
    "privacy.trackingprotection.socialtracking.enabled" = true;
  };

  defaultSettings =
    disableTelemetry
    // {
      "app.update.auto" = true;
      "browser.bookmarks.showMobileBookmarks" = true;
      "browser.ctrlTab.recentlyUsedOrder" = false;
      "browser.proton.enabled" = true;
      "browser.newtabpage.enabled" = true;

      # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
      "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" =
        false;

      "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
      "browser.search.region" = "US";
      "browser.search.suggest.enabled" = true;
      "browser.send_pings" = false;
      "browser.startup.homepage" = "https://lobste.rs";

      # 0 = Normal; 1 = Compact; 2 = Touch
      # FIXME: does this conflict with lepton?
      # "browser.uidensity" = 1;

      "browser.urlbar.placeholderName" = "…";
      "browser.urlbar.showSearchSuggestionsFirst" = false;
      "browser.urlbar.suggest.calculator" = true;
      "browser.urlbar.suggest.history" = true;

      "devtools.theme" = config.colorscheme.kind;

      "extensions.pocket.enabled" = false;

      # Allow extensions to run on Mozilla domains.
      # Required for Tridactyl and Dark Reader support on those pages.
      # See https://github.com/tridactyl/tridactyl/issues/1800
      "extensions.webextensions.restrictedDomains" = "";

      # FIXME: use global font defaults
      "font.default.x-western" = "sans-serif";
      "font.name.monospace.x-western" = themeFonts.mono.family;
      "font.name.sans-serif.x-western" = themeFonts.sans.family;
      "font.name.serif.x-western" = themeFonts.serif.family;
      "font.size.monospace.x-western" = themeFonts.mono.size;

      "identity.fxaccounts.account.device.name" = hostName;

      # CSS blur filter in v88+
      "layout.css.backdrop-filter.enabled" = true;

      # 0 = Accept all cookies by default
      # 1 = Only accept from the originating site (block third-party cookies)
      # 2 = Block all cookies by default
      # 3 = Block cookies from unvisited sites
      # 4 = New Cookie Jar policy (prevent storage access to trackers)
      "network.cookie.cookieBehavior" = 1;

      # Disable fingerprinting on AMO for Tridactyl.
      # See https://github.com/tridactyl/tridactyl/issues/1800
      "privacy.resistFingerprinting.block_mozAddonManager" = true;

      "security.enterprise_roots.enabled" = true;
      "services.sync.declinedEngines" = "addons,prefs,creditcards,addresses,tabs,passwords";
      "services.sync.engine.addons" = false;
      "services.sync.engine.passwords" = false;
      "services.sync.engine.prefs" = false;
      "services.sync.engineStatusChanged.addons" = true;
      "services.sync.engineStatusChanged.prefs" = true;
      "signon.rememberSignons" = false;

      # https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-context-properties
      "svg.context-properties.content.enabled" = true;

      # Enable custom stylesheets.
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    };
in {
  xdg.configFile."tridactyl".source = ./tridactyl;

  home.file = {
    "${homeProfilePath}/chrome/icons".source = leptonPath + "/icons";
    "${homeProfilePath}/user.js".source = makeProfileSettingsFile "home";

    "${workProfilePath}/chrome/icons".source = leptonPath + "/icons";
    "${workProfilePath}/user.js".source = makeProfileSettingsFile "work";
  };

  programs.buku.enable = true;
  programs.buku.enableBrowserIntegration = true;

  programs.firefox = {
    enable = true;
    package =
      if isDarwin
      then pkgs.runCommand "firefox-0.0.0" {} "mkdir $out"
      # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/networking/browsers/firefox/wrapper.nix
      else
        pkgs.firefox-wayland.override {
          cfg = {
            # Gnome shell native connector
            enableGnomeExtensions = moduleArgs.osConfig.services.gnome.chrome-gnome-shell.enable;
            # Tridactyl native connector
            enableTridactylNative = true;
            # Buku bookmarking tool native connector
            enableBukubrow = isBukuEnabled;
          };
        };

    # TODO: add zotero connector addon -- not available in upstream nur repo
    extensions = with firefox-addons; [
      onepassword-password-manager
      a11ycss
      add-custom-search-engine
      (lib.mkIf isBukuEnabled bukubrow)
      copy-selection-as-markdown
      darkreader
      display-_anchors
      firefox-color
      (lib.mkIf config.programs.browserpass.enable browserpass)
      flagfox
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

    profiles.home = {
      inherit userChrome userContent;
      id = 0;
      settings =
        defaultSettings
        // privacySettings
        // leptonSettings;
    };

    profiles.work = {
      inherit userChrome userContent;
      id = 1;
      settings =
        defaultSettings
        // {
          "browser.startup.homepage" = "about:blank";
          "browser.urlbar.placeholderName" = "Search";
          "privacy.trackingprotection.enabled" = true;
        };
    };
  };
}
##: References
#
# - https://github.com/cmacrae/config/blob/5a32507753339a2ee45155b78b76fda0824002a0/modules/macintosh.nix#L331-L407
# - https://restoreprivacy.com/firefox-privacy/
