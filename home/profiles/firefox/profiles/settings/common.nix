{
  inputs,
  theme,
  osConfig,
}: let
  inherit (theme) fonts;
  l = inputs.nixpkgs.lib // builtins;
  hostName = osConfig.networking.hostName or (l.getEnv "HOSTNAME");
in {
  "app.update.auto" = true;

  "browser.aboutConfig.showWarning" = false;
  "browser.bookmarks.showMobileBookmarks" = true;
  # "browser.display.use_document_fonts" = "1"; # default => "1"
  "browser.ctrlTab.recentlyUsedOrder" = false;
  "browser.fullscreen.exit_on_escape" = false;
  "browser.proton.enabled" = true;
  "browser.newtabpage.enabled" = true;
  # TODO: these are the default values -- customise them?
  # "browser.newtabpage.activity-stream.default.sites" = l.concatStringsSep "," [
  #   "https://www.youtube.com/"
  #   "https://www.facebook.com/"
  #   "https://www.amazon.com/"
  #   "https://www.reddit.com/"
  #   "https://www.wikipedia.org/"
  #   "https://twitter.com/"
  # ];

  # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
  "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;

  "browser.privatebrowsing.autostart" = false;

  "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
  "browser.search.region" = "US";
  "browser.search.suggest.enabled" = true;

  "browser.send_pings" = false;
  # "browser.ssl_override_behavior" = 2; default => 2

  "browser.urlbar.placeholderName" = "…";
  "browser.urlbar.showSearchSuggestionsFirst" = false;
  "browser.urlbar.suggest.calculator" = true;
  "browser.urlbar.suggest.history" = true;

  "browser.xul.error_pages.expert_bad_cert" = true;

  "devtools.theme" = "auto";
  "devtools.cache.disabled" = true;
  "devtools.defaultColorUnit" = "authored";
  "devtools.editor.tabsize" = 2;

  "extensions.pocket.enabled" = false;
  # Allow extensions to run on Mozilla domains.
  # Required for Tridactyl and Dark Reader support on those pages.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "extensions.webextensions.restrictedDomains" = "";

  "font.default.x-unicode" = fonts.sans.family;
  "font.default.x-western" = fonts.sans.family;
  # TODO
  # "font.language.group" = "x-unicode";
  "font.name.monospace.x-unicode" = fonts.mono.family;
  "font.name.monospace.x-western" = fonts.mono.family;
  "font.name.sans-serif.x-unicode" = fonts.sans.family;
  "font.name.sans-serif.x-western" = fonts.sans.family;
  "font.name.serif.x-unicode" = fonts.serif.family;
  "font.name.serif.x-western" = fonts.serif.family;
  "font.size.monospace.x-western" = fonts.mono.size;

  "identity.fxaccounts.account.device.name" = hostName;

  # CSS blur filter in v88+
  "layout.css.backdrop-filter.enabled" = true;

  "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;

  "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  "privacy.trackingprotection.enabled" = true;
  "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
  "privacy.trackingprotection.socialtracking.enabled" = true;
  # Disable fingerprinting on AMO to allow for Tridactyl control.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "privacy.resistFingerprinting.block_mozAddonManager" = true;

  "security.enterprise_roots.enabled" = true;
  "services.sync.declinedEngines" = "addons,prefs,creditcards,addresses,passwords";
  "services.sync.engine.addons" = false;
  "services.sync.engine.passwords" = false;
  "services.sync.engine.prefs" = false;
  "services.sync.engine.tabs" = true;
  "services.sync.engineStatusChanged.addons" = true;
  "services.sync.engineStatusChanged.prefs" = true;

  "signon.rememberSignons" = false;

  # Required for Lepton icon support.
  # https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-context-properties
  "svg.context-properties.content.enabled" = true;

  # Enable custom stylesheets.
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
}
