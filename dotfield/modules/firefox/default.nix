{ config, lib, pkgs, ... }:

# References:
# - https://github.com/cmacrae/config/blob/5a32507753339a2ee45155b78b76fda0824002a0/modules/macintosh.nix#L331-L407

with lib;
let
  cfg = config.my.modules.firefox;
  configDir = "${config.dotfield.configDir}/firefox";

  addons = pkgs.nur.repos.rycee.firefox-addons;
in {
  options = with lib; {
    my.modules.firefox = { enable = mkEnableOption false; };
  };

  config = mkIf cfg.enable {
    my.hm.file = {
      # Provide extensions with a JavaScript binding for GPG.
      # TODO: enable conditionally with gpg module
      gpgmejson = {
        target =
          "Library/Application Support/Mozilla/NativeMessagingHosts/gpgmejson.json";
        source = (builtins.toJSON {
          name = "gpgmejson";
          description = "JavaScript binding for GnuPG";
          # FIXME: error: attribute 'bin' missing
          path = "${pkgs.gpgme.bin}/gpg-json";
          type = "stdio";
          allowed_extensions = "jid1-AQqSMBYb0a8ADg@jetpack";
        });
      };
    };

    my.hm.programs.firefox = {
      enable = true;
      # Handled by the Homebrew module
      # This populates a dummy package to satisfy the requirement
      package = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";

      # FIXME: only the addons available in the nur repo will work!
      extensions = with addons; [
        # 1password-x-password-manager
        # copy-selection-as-markdown
        darkreader
        # display-_anchors
        firefox-color
        mailvelope
        multi-account-containers
        octolinker
        old-reddit-redirect
        # org-capture
        # pinboard
        privacy-redirect
        react-devtools
        reddit-enhancement-suite
        reduxdevtools
        refined-github
        sidebery
        # tridactyl
        ublock-origin
        vimium
      ];

      profiles = let
        defaultSettings = {
          "app.update.auto" = false;
          "browser.bookmarks.showMobileBookmarks" = true;
          "browser.ctrlTab.recentlyUsedOrder" = false;
          "browser.newtabpage.enabled" = false;
          "browser.search.hiddenOneOffs" =
            "Google,Yahoo,Bing,Amazon.com,Twitter";
          "browser.search.region" = "US";
          "browser.search.suggest.enabled" = false;
          "browser.send_pings" = false;
          "browser.startup.homepage" = "https://lobste.rs";
          # 0 = Normal; 1 = Compact; 2 = Touch
          "browser.uidensity" = 1;
          "browser.urlbar.placeholderName" = "…";
          "browser.urlbar.showSearchSuggestionsFirst" = false;
          "browser.urlbar.suggest.history" = false;
          # TODO: set devtools theme
          # https://rycee.gitlab.io/nur-expressions/hm-options.html
          # "devtools.theme" = "${config.theme.base16.kind}";
          "extensions.pocket.enabled" = false;
          "font.default.x-western" = "sans-serif";
          "font.name.monospace.x-western" = "PragmataPro Liga";
          "font.name.sans-serif.x-western" = "Inter";
          "font.size.monospace.x-western" = 16;
          "identity.fxaccounts.account.device.name" =
            config.networking.hostName;
          "network.dns.disablePrefetch" = true;
          "privacy.donottrackheader.enabled" = true;
          "privacy.donottrackheader.value" = 1;
          "privacy.trackingprotection.enabled" = true;
          "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
          "privacy.trackingprotection.socialtracking.enabled" = true;
          "services.sync.declinedEngines" = "addons,passwords,prefs";
          "services.sync.engine.addons" = false;
          "services.sync.engine.passwords" = false;
          "services.sync.engine.prefs" = false;
          "services.sync.engineStatusChanged.addons" = true;
          "services.sync.engineStatusChanged.prefs" = true;
          "signon.rememberSignons" = false;
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
      in {
        home = {
          id = 0;
          settings = defaultSettings;
          userChrome = (builtins.readFile (pkgs.substituteAll {
            name = "homeUserChrome";
            src = "${configDir}/userChrome.css";
            # TODO: handle profile color differentiation
            # tabLineColour = "#5e81ac";
          }));
          userContent = userContentCSS;
        };

        work = {
          id = 1;
          settings = defaultSettings // {
            "browser.startup.homepage" = "about:blank";
            "browser.urlbar.placeholderName" = "Google";
          };
          userChrome = (builtins.readFile (pkgs.substituteAll {
            name = "workUserChrome";
            src = "${configDir}/userChrome.css";
            # TODO: handle profile color differentiation
            # tabLineColour = "#d08770";
          }));
          userContent = userContentCSS;
        };
      };
    };
  };
}
