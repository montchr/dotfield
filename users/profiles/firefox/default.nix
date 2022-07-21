moduleArgs @ {
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (pkgs.nur.repos.rycee) firefox-addons;

  themeFonts = config.theme.font;

  isBukuEnabled = config.programs.buku.enable && config.programs.buku.enableBrowserIntegration;

  hostName = moduleArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");
  lepton = import ./lepton.nix;

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
      "browser.uidensity" = 1;

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

  styles = {
    dotfield = {
      userChrome = ''
        /* Load Dotfield customisations. */
        /* Note: the entire content of this file is copied below, so the file itself isn't imported. */
        /* @import url("${toString ./userChrome.css}"); */

        * {
          font-family: ${themeFonts.mono.family}, monospace !important;
          /* font-size: ${builtins.toString themeFonts.mono.size}px; */
        }
      '';
      userContent = ''
        /* Load Dotfield customisations. */
        @import url("${toString ./userContent.css}");
      '';
    };
    lepton = {
      userChrome = ''
        /* Load Lepton userChrome.css */
        @import url("${inputs.firefox-lepton.outPath}/userChrome.css");
      '';
      userContent = ''
        /* Load Lepton userContent.css */
        @import url("${inputs.firefox-lepton.outPath}/userContent.css");
      '';
    };
  };
in {
  xdg.configFile."tridactyl".source = ./tridactyl;

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
      old-reddit-redirect
      org-capture
      pinboard
      privacy-badger
      react-devtools
      reddit-enhancement-suite
      reduxdevtools
      refined-github
      single-file
      tab-session-manager
      tabliss
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
      id = 0;

      settings =
        defaultSettings
        // privacySettings
        // lepton.settings.required
        // lepton.settings.theme.lepton
        // lepton.settings.recommended
        // lepton.settings.optional;

      userChrome = ''
        ${styles.lepton.userChrome}

        :root {
          --dotfield-tab-line-color: #5e81ac;
        }

        ${styles.dotfield.userChrome}
      '';

      userContent = ''
        ${styles.lepton.userContent}
        ${styles.dotfield.userContent}
      '';
    };

    profiles.work = {
      id = 1;

      settings =
        defaultSettings
        // lepton.settings.required
        // lepton.settings.theme.lepton
        // {
          "browser.startup.homepage" = "about:blank";
          "browser.urlbar.placeholderName" = "Search";

          # 0 = Accept all cookies by default
          # 1 = Only accept from the originating site (block third-party cookies)
          # 2 = Block all cookies by default
          # 3 = Block cookies from unvisited sites
          # 4 = New Cookie Jar policy (prevent storage access to trackers)
          "network.cookie.cookieBehavior" = 0;

          "privacy.trackingprotection.enabled" = false;
        };

      userChrome = ''
        ${styles.lepton.userChrome}
        ${styles.dotfield.userChrome}
      '';

      userContent = ''
        ${styles.dotfield.userContent}
      '';
    };
  };
}
##: References
#
# - https://github.com/cmacrae/config/blob/5a32507753339a2ee45155b78b76fda0824002a0/modules/macintosh.nix#L331-L407
# - https://restoreprivacy.com/firefox-privacy/

