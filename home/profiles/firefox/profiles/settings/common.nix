hmArgs @ {
  inputs,
  config,
  ...
}: let
  inherit (config) colorscheme;
  inherit (config.theme) fonts;
  l = inputs.nixpkgs.lib // builtins;
  hostName = hmArgs.osConfig.networking.hostName or (l.getEnv "HOSTNAME");
in {
  "app.update.auto" = true;
  "browser.bookmarks.showMobileBookmarks" = true;
  "browser.ctrlTab.recentlyUsedOrder" = false;
  "browser.proton.enabled" = true;
  "browser.newtabpage.enabled" = true;

  # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
  "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;

  "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
  "browser.search.region" = "US";
  "browser.search.suggest.enabled" = true;
  "browser.send_pings" = false;
  "browser.startup.homepage" = "https://lobste.rs";

  "browser.urlbar.placeholderName" = "…";
  "browser.urlbar.showSearchSuggestionsFirst" = false;
  "browser.urlbar.suggest.calculator" = true;
  "browser.urlbar.suggest.history" = true;

  "devtools.theme" = colorscheme.kind;

  "extensions.pocket.enabled" = false;

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

  "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  "privacy.trackingprotection.enabled" = true;
  "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
  "privacy.trackingprotection.socialtracking.enabled" = true;

  # Disable fingerprinting on AMO for Tridactyl.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "privacy.resistFingerprinting.block_mozAddonManager" = true;

  "security.enterprise_roots.enabled" = true;
  # FIXME: sync tabs
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
}
