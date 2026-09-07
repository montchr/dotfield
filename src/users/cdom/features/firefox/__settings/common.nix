# TODO: add docs for all "magical" numeric values
{
  "browser.aboutConfig.showWarning" = false;

  "browser.ai.control.default" = "blocked";
  "browser.ai.control.linkPreviewKeyPoints" = "blocked";
  "browser.ai.control.pdfjsAltText" = "blocked";
  "browser.ai.control.sidebarChatbot" = "blocked";
  "browser.ai.control.smartTabGroups" = "blocked";
  "browser.ai.control.smartWindow" = "blocked";
  "browser.ai.control.translations" = "blocked";

  "browser.bookmarks.editDialog.firstEditField" = "tagsField";
  "browser.bookmarks.restore_default_bookmarks" = false;
  "browser.bookmarks.showMobileBookmarks" = true;

  "browser.contentblocking.category" = "strict";
  "browser.ctrlTab.recentlyUsedOrder" = true;
  # TODO: add documentation for values
  # "browser.display.os-zoom-behavior" = 1;

  "browser.proton.enabled" = true;

  "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
  # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
  "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;
  "browser.newtabpage.activity-stream.showSponsored" = false;
  "browser.newtabpage.activity-stream.showSponsoredCheckboxes" = false;
  "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
  "browser.newtabpage.activity-stream.topSitesRows" = 2;

  "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
  "browser.search.region" = "US";
  "browser.search.suggest.enabled" = true;

  "browser.smartwindow.memories.generateFromConversation" = false;
  "browser.smartwindow.memories.generateFromHistory" = false;

  # TODO: add documentation for values
  "browser.startup.page" = 3;

  "browser.tabs.groups.smart.enabled" = false;
  "browser.tabs.groups.smart.userEnabled" = false;
  "browser.tabs.insertAfterCurrent" = true;
  # Whether we should draw the tabs on top of the titlebar.
  # no (0), yes (1), or default (2), which is true everywhere except Linux.
  # <https://searchfox.org/mozilla-release/rev/b6792379492929d65f5b61a3caa0a9b19bbd7078/modules/libpref/init/StaticPrefList.yaml#1565-1571>
  "browser.tabs.inTitlebar" = 0;
  # Silence this incessant force-feeding of unwanted UI flourishes.
  "browser.tabs.splitview.hasUsed" = true;
  # Warn when closing *multiple* tabs.
  "browser.tabs.warnOnClose" = true;

  # Communicates the toolbar color to platform (for e.g., prefers-color-scheme).
  #
  # Returns whether the toolbar is dark (0), light (1), or system (2). The
  # theming code overrides it if appropriate.
  "browser.theme.toolbar-theme" = 2;

  # UI density of the browser chrome. This mostly affects toolbarbutton
  # and urlbar spacing. The possible values are 0=normal, 1=compact, 2=touch.
  # https://searchfox.org/firefox-release/rev/c749d31276fecaa969469c4a53889a5639bb415c/browser/app/profile/firefox.js#255-257
  "browser.uidensity" = 1;

  "browser.urlbar.placeholderName" = "…";
  "browser.urlbar.showSearchSuggestionsFirst" = false;
  "browser.urlbar.speculativeConnect.enabled" = false;
  "browser.urlbar.suggest.calculator" = true;
  "browser.urlbar.suggest.history" = true;
  "browser.urlbar.suggest.quicksuggest.all" = false;
  "browser.urlbar.suggest.quicksuggest.sponsored" = false;

  "devtools.theme" = "auto";
  "devtools.cache.disabled" = true;

  # Allow extensions to run on Mozilla domains.
  # Required for Tridactyl and Dark Reader support on those pages.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "extensions.webextensions.restrictedDomains" = "";

  "identity.fxaccounts.enabled" = true;

  # CSS blur filter in v88+
  "layout.css.backdrop-filter.enabled" = true;

  # Follow system color theme.
  # FIXME: this value doesn't work on macOS (Linux unverified)
  # TODO: add documentation for values (where is it? who knows?)
  # "layout.css.prefers-color-scheme.content-override" = 2;

  # Enable picture-in-picture (PiP) mode.
  "media.videocontrols.picture-in-picture.enable-when-switching-tabs.enabled" = false;
  "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
  "extensions.pictureinpicture.enable_picture_in_picture_overrides" = true;

  # WARNING: changing this may break keyboard shortcuts.
  "permissions.default.shortcuts" = 0; # :: default => 0

  "privacy.clearOnShutdown.history" = false;
  "privacy.clearOnShutdown.downloads" = false;

  # Disable fingerprinting on AMO to allow for Tridactyl control.
  # See https://github.com/tridactyl/tridactyl/issues/1800
  "privacy.resistFingerprinting.block_mozAddonManager" = true;

  "services.sync.engine.passwords" = false;
  "services.sync.declinedEngines" = "creditcards,addresses,passwords";

  "signon.autofillForms" = false;
  "signon.generation.enabled" = false;
  "signon.rememberSignons" = false;

  # Required for Lepton icon support.
  # https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-context-properties
  "svg.context-properties.content.enabled" = true;

  # Enable custom stylesheets.
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

  "webgl.disabled" = false;

  # Firefox 150+: Disable GTK emoji picker, which is force-bound to [Ctrl-.]
  "widget.gtk.native-emoji-dialog" = false;

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
