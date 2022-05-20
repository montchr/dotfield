moduleArgs @ {
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  hostName = moduleArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");

  leptonDir = inputs.firefox-lepton.outPath;
  addons = pkgs.nur.repos.rycee.firefox-addons;

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

      "font.default.x-western" = "sans-serif";
      "font.name.monospace.x-western" = "PragmataPro";
      "font.name.sans-serif.x-western" = "Public Sans";
      "font.size.monospace.x-western" = 18;
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
        @import url("${toString ./userChrome.css}");
      '';
      userContent = ''
        :root {
          --tridactyl-font-family: "PragmataPro Liga" !important;
          --tridactyl-cmdl-font-family: "PragmataPro Mono Liga" !important;
          --tridactyl-status-font-family: "PragmataPro Mono" !important;
          --tridactyl-cmplt-font-family: "PragmataPro Mono" !important;
          --tridactyl-hintspan-font-family: "PragmataPro Mono" !important;
        }
      '';
    };
    lepton = {
      userChrome = ''
        /* Load Lepton userChrome.css */
        @import url("${leptonDir}/userChrome.css");
      '';
      userContent = ''
        /* Load Lepton userContent.css */
        @import url("${leptonDir}/userContent.css");
      '';
    };
  };

  userContent = ''
    ${styles.lepton.userContent}
    ${styles.dotfield.userContent}
  '';
in
  lib.mkMerge [
    {
      xdg.configFile."tridactyl".source = ./tridactyl;

      programs.firefox = {
        enable = true;

        extensions = with addons; [
          onepassword-password-manager
          a11ycss
          add-custom-search-engine
          copy-selection-as-markdown
          darkreader
          display-_anchors
          firefox-color

          (lib.mkIf config.programs.browserpass.enable browserpass)

          # Flagfox by Dave G
          #
          # Displays a country flag depicting the location of the current website's
          # server and provides a multitude of tools such as site safety checks,
          # whois, translation, similar sites, validation, URL shortening, and
          # more...
          #
          # https://addons.mozilla.org/en-US/firefox/addon/flagfox/
          flagfox

          floccus

          # Light/Dark theme switcher for Firefox by Remy Sharp
          #
          # https://github.com/remy/light-dark-switcher
          #
          # FIXME: appears to be removed from firefox addons repo?
          # light-dark-switcher

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

          # Return Youtube Dislike by Dmitry Selivanov
          #
          # Returns ability to see dislike statistics on youtube
          #
          # https://addons.mozilla.org/en-US/firefox/addon/return-youtube-dislikes/
          search-engines-helper

          # SingleFile by gildas
          #
          # Save an entire web page—including images and styling—as a single HTML file.
          #
          # https://addons.mozilla.org/en-US/firefox/addon/single-file/
          single-file

          tab-session-manager
          tabliss
          temporary-containers
          tridactyl
          ublock-origin

          ##: Themes {{

          # Arctic Nord Theme by christos
          #
          # https://addons.mozilla.org/en-US/firefox/addon/arctic-nord-theme/
          #
          # TODO: add this to upstream repo
          # arctic-nord-theme

          # Nord Polar Night Theme by christos
          #
          # https://addons.mozilla.org/en-US/firefox/addon/nord-polar-night-theme/
          theme-nord-polar-night

          ##: }}
        ];

        profiles.home = {
          inherit userContent;

          id = 0;
          settings = defaultSettings // privacySettings;
          userChrome = ''
            ${styles.lepton.userChrome}

            :root {
              --dotfield-tab-line-color: #5e81ac;
            }

            ${styles.dotfield.userChrome}
          '';
        };

        profiles.work = {
          inherit userContent;

          id = 1;

          settings =
            defaultSettings
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

            :root {
              --dotfield-color-alley-red: #a22e29;
              --dotfield-tab-line-color: var(--dotfield-color-alley-red);
            }

            ${styles.dotfield.userChrome}
          '';
        };
      };
    }

    (lib.mkIf isDarwin {
      # Handled by the Homebrew module
      # This populates a dummy package to satisfy the requirement
      programs.firefox.package = pkgs.runCommand "firefox-0.0.0" {} "mkdir $out";
    })
  ]

##: References
#
# - https://github.com/cmacrae/config/blob/5a32507753339a2ee45155b78b76fda0824002a0/modules/macintosh.nix#L331-L407
# - https://restoreprivacy.com/firefox-privacy/

