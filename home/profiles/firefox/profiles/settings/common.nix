{
  inputs,
  theme,
  osConfig,
  ...
}: let
  inherit (theme) fonts;
  l = inputs.nixpkgs.lib // builtins;
  hostName = osConfig.networking.hostName or (l.getEnv "HOSTNAME");
in {
  # "browser.bookmarks.showMobileBookmarks" = true;
  "browser.contentblocking.category" = "strict";
  "browser.ctrlTab.recentlyUsedOrder" = false;
  "browser.display.os-zoom-behavior" = 1;
  # "browser.fullscreen.exit_on_escape" = false;
  "browser.proton.enabled" = true;
  "browser.newtabpage.enabled" = true;

  # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
  "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;

  "browser.safebrowsing.appRepURL" = "";
  "browser.safebrowsing.malware.enabled" = false;

  "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
  "browser.search.region" = "US";
  "browser.search.suggest.enabled" = true;

  "browser.send_pings" = false;
  "browser.startup.page" = 3;
  "browser.uidensity" = 1; # Dense.
  "browser.urlbar.placeholderName" = "…";
  "browser.urlbar.showSearchSuggestionsFirst" = false;
  "browser.urlbar.speculativeConnect.enabled" = false;
  "browser.urlbar.suggest.calculator" = true;
  "browser.urlbar.suggest.history" = true;

  "devtools.theme" = "auto";
  # "devtools.cache.disabled" = true;

  "dom.security.https_only_mode" = true;

  "experiments.activeExperiment" = false;
  "experiments.enabled" = false;
  "experiments.supported" = false;

  # Allow extensions to run on Mozilla domains.
  # Required for Tridactyl and Dark Reader support on those pages.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "extensions.webextensions.restrictedDomains" = "";

  "font.default.x-western" = fonts.sans.family;
  "font.name.monospace.x-western" = fonts.mono.family;
  "font.name.sans-serif.x-western" = fonts.sans.family;
  "font.name.serif.x-western" = fonts.serif.family;

  "font.size.monospace.x-western" = fonts.mono.size;

  "identity.fxaccounts.account.device.name" = hostName;

  # CSS blur filter in v88+
  "layout.css.backdrop-filter.enabled" = true;

  # Follow system color theme.
  "layout.css.prefers-color-scheme.content-override" = 2;

  "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;

  "network.allow-experiments" = false;
  "network.dns.disablePrefetch" = true;
  "network.http.referer.XOriginPolicy" = 2;
  "network.http.referer.XOriginTrimmingPolicy" = 2;
  "network.http.referer.trimmingPolicy" = 1;
  "network.prefetch-next" = false;

  "permissions.default.shortcuts" = 2; # Don't steal my shortcuts!

  "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  # "privacy.firstparty.isolate" = true;
  "privacy.trackingprotection.enabled" = true;
  "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
  "privacy.trackingprotection.socialtracking.enabled" = true;
  # Disable fingerprinting on AMO to allow for Tridactyl control.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "privacy.resistFingerprinting.block_mozAddonManager" = true;

  "services.sync.engine.passwords" = false;

  "signon.rememberSignons" = false;

  # Required for Lepton icon support.
  # https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-context-properties
  "svg.context-properties.content.enabled" = true;

  # Enable custom stylesheets.
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

  "ui.textScaleFactor" = 100;

  # Fully disable Pocket. See
  # https://www.reddit.com/r/linux/comments/zabm2a.
  "extensions.pocket.enabled" = false;
  "extensions.pocket.api" = "0.0.0.0";
  "extensions.pocket.loggedOutVariant" = "";
  "extensions.pocket.oAuthConsumerKey" = "";
  "extensions.pocket.onSaveRecs" = false;
  "extensions.pocket.onSaveRecs.locales" = "";
  "extensions.pocket.showHome" = false;
  "extensions.pocket.site" = "0.0.0.0";
  "browser.newtabpage.activity-stream.pocketCta" = "";
  "browser.newtabpage.activity-stream.section.highlights.includePocket" =
    false;
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.section.highlights.includePocket" =
    false;
}
##: Sources:
# - https://git.sr.ht/~rycee/configurations/tree/5ef3e3b2bd400841be7ec641812b8006191bb7fc/item/user/firefox.nix

