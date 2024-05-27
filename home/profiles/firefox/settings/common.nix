# TODO: add docs for all "magical" numeric values
{
  theme,
  osConfig,
  lib,
  ...
}:
let
  # FIXME: only do theme stuff if theme enabled for user
  inherit (theme) fonts;
  hostName = osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");
in
{
  "browser.bookmarks.showMobileBookmarks" = true;
  "browser.contentblocking.category" = "strict";
  "browser.ctrlTab.recentlyUsedOrder" = false;
  # TODO: add documentation for values
  # "browser.display.os-zoom-behavior" = 1;
  "browser.proton.enabled" = true;
  # "browser.newtabpage.enabled" = true;

  # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
  "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;

  "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
  "browser.search.region" = "US";
  "browser.search.suggest.enabled" = true;

  # TODO: add documentation for values
  "browser.startup.page" = 3;

  # Whether we should draw the tabs on top of the titlebar.
  # no (0), yes (1), or default (2), which is true everywhere except Linux.
  # <https://searchfox.org/mozilla-release/rev/b6792379492929d65f5b61a3caa0a9b19bbd7078/modules/libpref/init/StaticPrefList.yaml#1565-1571>
  "browser.tabs.inTitlebar" = lib.mkDefault 0;

  # TODO: add documentation for values
  "browser.uidensity" = 1; # Dense.
  "browser.urlbar.placeholderName" = "…";
  "browser.urlbar.showSearchSuggestionsFirst" = false;
  "browser.urlbar.speculativeConnect.enabled" = false;
  "browser.urlbar.suggest.calculator" = true;
  "browser.urlbar.suggest.history" = true;

  "devtools.theme" = "auto";
  "devtools.cache.disabled" = true;

  # Allow extensions to run on Mozilla domains.
  # Required for Tridactyl and Dark Reader support on those pages.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "extensions.webextensions.restrictedDomains" = "";

  # Default font should be a generic, either 'serif' or 'sans-serif'.
  "font.default.x-western" = "sans-serif";
  "font.name.monospace.x-western" = fonts.monospace.name;
  "font.name.sans-serif.x-western" = fonts.sansSerif.name;
  "font.name.serif.x-western" = fonts.serif.name;
  "font.size.monospace.x-western" = 12;

  "identity.fxaccounts.account.device.name" = hostName;

  # CSS blur filter in v88+
  "layout.css.backdrop-filter.enabled" = true;

  # Follow system color theme.
  # FIXME: this value doesn't work on macOS (Linux unverified)
  # TODO: add documentation for values (where is it? who knows?)
  # "layout.css.prefers-color-scheme.content-override" = 2;

  "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;

  # WARNING: changing this may break keyboard shortcuts.
  "permissions.default.shortcuts" = 0; # :: default => 0

  "privacy.donottrackheader.enabled" = true;
  # TODO: add docs for values
  "privacy.donottrackheader.value" = 1;
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
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.section.highlights.includePocket" =
    false;
}
##: Sources:
# - https://git.sr.ht/~rycee/configurations/tree/5ef3e3b2bd400841be7ec641812b8006191bb7fc/item/user/firefox.nix
