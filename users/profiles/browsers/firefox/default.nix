{ config, lib, pkgs, inputs, ... }:

with lib;

let
  leptonDir = inputs.firefox-lepton.outPath;
  addons = pkgs.nur.repos.rycee.firefox-addons;

  privacySettings = {
    "network.dns.disablePrefetch" = true;
    "privacy.donottrackheader.enabled" = true;
    "privacy.donottrackheader.value" = 1;
    "privacy.trackingprotection.enabled" = true;
    "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
    "privacy.trackingprotection.socialtracking.enabled" = true;
  };

  defaultSettings = {
    "app.update.auto" = true;
    "browser.bookmarks.showMobileBookmarks" = true;
    "browser.ctrlTab.recentlyUsedOrder" = false;
    "browser.proton.enabled" = true;
    "browser.newtabpage.enabled" = true;

    # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
    "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" =
      false;

    "browser.search.hiddenOneOffs" =
      "Google,Yahoo,Bing,Amazon.com,Twitter";
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

    # TODO: set devtools theme
    # https://rycee.gitlab.io/nur-expressions/hm-options.html
    # "devtools.theme" = "${config.theme.base16.kind}";

    "extensions.pocket.enabled" = false;

    # Allow extensions to run on Mozilla domains.
    # Required for Tridactyl and Dark Reader support on those pages.
    # See https://github.com/tridactyl/tridactyl/issues/1800
    "extensions.webextensions.restrictedDomains" = "";

    "font.default.x-western" = "sans-serif";
    "font.name.monospace.x-western" = "PragmataPro Liga";
    "font.name.sans-serif.x-western" = "Inter";
    "font.size.monospace.x-western" = 16;
    "identity.fxaccounts.account.device.name" =
      config.networking.hostName;

    # CSS blur filter in v88+
    "layout.css.backdrop-filter.enabled" = true;

    # Disable fingerprinting on AMO for Tridactyl.
    # See https://github.com/tridactyl/tridactyl/issues/1800
    "privacy.resistFingerprinting.block_mozAddonManager" = true;

    "security.enterprise_roots.enabled" = true;
    "services.sync.declinedEngines" = "addons,passwords,prefs";
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

  userContentCSS = ''
    :root {
      --tridactyl-font-family: "PragmataPro Liga" !important;
      --tridactyl-cmdl-font-family: "PragmataPro Mono Liga" !important;
      --tridactyl-status-font-family: "PragmataPro Mono" !important;
      --tridactyl-cmplt-font-family: "PragmataPro Mono" !important;
      --tridactyl-hintspan-font-family: "PragmataPro Mono" !important;
    }
  '';

  imports = {
    lepton = {
      userChrome = ''
        /* Load Lepton userChrome.css */
        /* @import url("file://${leptonDir}/userChrome.css"); */
        @import url("${leptonDir}/userChrome.css");
      '';
      userContent = ''
        /* Load Lepton userContent.css */
        /* @import url("file://${leptonDir}/userContent.css"); */
        @import url("file://${leptonDir}/userContent.css");
      '';
    };
  };

  defaultUserContentStyles = ''
    ${imports.lepton.userContent}
    ${userContentCSS}
  '';
in

{
  my.hm.xdg.configFile."tridactyl".source = ./tridactyl;

  my.hm.programs.firefox = {
    enable = true;
    # Handled by the Homebrew module
    # This populates a dummy package to satisfy the requirement
    package = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";

    extensions = with addons; [
      onepassword-password-manager
      a11ycss
      add-custom-search-engine
      copy-selection-as-markdown
      darkreader
      display-_anchors
      firefox-color
      floccus
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
      search-engines-helper
      tab-session-manager
      tabliss
      temporary-containers
      tridactyl
      ublock-origin
    ];

    profiles.home = {
      id = 0;
      settings = defaultSettings // privacySettings;
      userChrome = ''
        ${imports.lepton.userChrome}

        :root {
          --dotfield-tab-line-color: #5e81ac;
        }

        ${builtins.readFile ./userChrome.css}
      '';

      userContent = defaultUserContentStyles;
    };

    profiles.work = {
      id = 1;

      settings = defaultSettings // {
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
        ${imports.lepton.userChrome}

        :root {
          --dotfield-color-alley-red: #a22e29;
          --dotfield-tab-line-color: var(--dotfield-color-alley-red);
        }

        ${builtins.readFile ./userChrome.css}
      '';

      userContent = defaultUserContentStyles;
    };

  };
}

# References:
# - https://github.com/cmacrae/config/blob/5a32507753339a2ee45155b78b76fda0824002a0/modules/macintosh.nix#L331-L407
# - https://restoreprivacy.com/firefox-privacy/
