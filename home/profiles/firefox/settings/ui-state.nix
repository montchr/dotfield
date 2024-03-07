# NOTE: The keys must not be changed! Only change the values.
#
# It's okay if not all items apply to all configurations, as they indicate the
# totality of possible items. Some of the extensions might not even be installed
# anymore, and that's okay.
#
# TODO: hide the stuff we don't care about so that only stuff like "nav-bar" and
# "unified-extensions-area" are accessible. all settings likely still need to be
# present in order to prevent config corruption
{ lib, ... }:
let
  navbarItems = [
    "back-button"
    "forward-button"
    "stop-reload-button"
    "urlbar-container"
    "save-to-pocket-button"
    "downloads-button"
    # Dark Reader
    "addon_darkreader_org-browser-action"
    # 1Password
    "_d634138d-c276-4fc8-924b-40a0ea21d284_-browser-action"
    # TODO: identify
    "_5158522f-7494-41b1-89ff-00d4cc1d87d3_-browser-action"
    # TODO: identify
    "jid0-adyhmvsp91nuo8prv0mn2vkeb84_jetpack-browser-action"
    # uBlock Origin
    "ublock0_raymondhill_net-browser-action"
    "add-ons-button"
    # TODO: only if bukubrow extension is requested/activated
    "bukubrow_samhh_com-browser-action"
    # TODO: identify
    "_af37054b-3ace-46a2-ac59-709e4412bec6_-browser-action"
    "display-anchors_robwu_nl-browser-action"
    "reset-pbm-toolbar-button"
    "offline-qr-code_rugk_github_io-browser-action"
    "_7a7a4a92-a2a0-41d1-9fd7-1e92480d612d_-browser-action"
    "_aecec67f-0d10-4fa7-b7c7-609a2db280cf_-browser-action"
    # aka the extensions flyout menu
    "unified-extensions-button"
  ];
  tabsToolbarItems = [
    # Firefox: "View recent browsing across windows and devices"
    "firefox-view-button"
    "tabbrowser-tabs"
    "new-tab-button"
    "alltabs-button"
    # Temporary Container: "Open a new tab in a new Temporary Container"
    "_c607c8df-14a7-4f28-894f-29e8722976af_-browser-action"
  ];
  extensionsFlyoutItems = [
    "browserpass_maximbaz_com-browser-action"
    # Firefox Multi-Account Containers
    "_testpilot-containers-browser-action"
    # Anchors Reveal
    "jid1-xx0tccgba7gvgw_jetpack-browser-action"
    # Tab Session Manager
    "tab-session-manager_sienori-browser-action"
    # GNOME Shell Integration
    "chrome-gnome-shell_gnome_org-browser-action"
    # Refined GitHub
    "_a4c4eda4-fb84-4a84-b4a1-f7c1cbf2a1ad_-browser-action"
    # Auto Tab Discard
    "_c2c003ee-bd69-42a2-b0e9-6f34222cb046_-browser-action"
    # Consent-O-Matic
    "gdpr_cavi_au_dk-browser-action"
    # Copy Selection As Markdown
    "_db9a72da-7bc5-4805-bcea-da3cb1a15316_-browser-action"
  ];
in
{
  # TODO: determine whether the order of keys matters...
  "browser.uiCustomization.state" = builtins.toJSON {
    currentVersion = 20;
    dirtyAreaCache = [
      "nav-bar"
      "PersonalToolbar"
      "toolbar-menubar"
      "TabsToolbar"
      "widget-overflow-fixed-list"
      "unified-extensions-area"
    ];
    newElementCount = 6;
    placements = {
      # Bookmarks Bar -- but the name makes it seem like there could be other items?
      PersonalToolbar = [ "personal-bookmarks" ];
      TabsToolbar = tabsToolbarItems;
      nav-bar = navbarItems;
      # Indication that the toolbar menu flyout is the same as the menubar items.
      toolbar-menubar = [ "menubar-items" ];
      unified-extensions-area = extensionsFlyoutItems;
      widget-overflow-fixed-list = [ "fxa-toolbar-menu-button" ];
    };
    # TODO: determine how much these matter -- firefox will likely want this
    # updated any time a new extension is added...
    seen = [
      "developer-button"
      "addon_darkreader_org-browser-action"
      "_d634138d-c276-4fc8-924b-40a0ea21d284_-browser-action"
      "_testpilot-containers-browser-action"
      "_5158522f-7494-41b1-89ff-00d4cc1d87d3_-browser-action"
      "_a4c4eda4-fb84-4a84-b4a1-f7c1cbf2a1ad_-browser-action"
      "ublock0_raymondhill_net-browser-action"
      "bukubrow_samhh_com-browser-action"
      "chrome-gnome-shell_gnome_org-browser-action"
      "_af37054b-3ace-46a2-ac59-709e4412bec6_-browser-action"
      "browserpass_maximbaz_com-browser-action"
      "display-anchors_robwu_nl-browser-action"
      "tab-session-manager_sienori-browser-action"
      "_c607c8df-14a7-4f28-894f-29e8722976af_-browser-action"
      "_aecec67f-0d10-4fa7-b7c7-609a2db280cf_-browser-action"
      "_7a7a4a92-a2a0-41d1-9fd7-1e92480d612d_-browser-action"
      "_c2c003ee-bd69-42a2-b0e9-6f34222cb046_-browser-action"
      "jid1-xx0tccgba7gvgw_jetpack-browser-action"
      "gdpr_cavi_au_dk-browser-action"
      "_db9a72da-7bc5-4805-bcea-da3cb1a15316_-browser-action"
      "jid0-adyhmvsp91nuo8prv0mn2vkeb84_jetpack-browser-action"
      "offline-qr-code_rugk_github_io-browser-action"
    ];
  };
}
