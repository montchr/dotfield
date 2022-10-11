moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
  inherit (pkgs.sources) firefox-lepton-ui;
  inherit
    (lib)
    concatStrings
    mapAttrsToList
    optional
    ;
  inherit
    (pkgs)
    firefox-wayland
    makeDesktopItem
    runCommand
    writeText
    ;
  inherit (config.theme) fonts;

  cfg = config.programs.firefox;

  hasGnomeShell = moduleArgs.osConfig.services.gnome.chrome-gnome-shell.enable or false;

  # via https://github.com/nix-community/home-manager/blob/e1f1160284198a68ea8c7fffbbb1436f99e46ef9/modules/programs/firefox.nix#L11-L20
  mozillaConfigPath =
    if isDarwin
    then "Library/Application Support/Mozilla"
    else ".mozilla";
  firefoxConfigPath =
    if isDarwin
    then "Library/Application Support/Firefox"
    else "${mozillaConfigPath}/firefox";
  profilesPath =
    if isDarwin
    then "${firefoxConfigPath}/Profiles"
    else firefoxConfigPath;

  homeProfilePath = "${profilesPath}/${cfg.profiles.home.path}";
  workProfilePath = "${profilesPath}/${cfg.profiles.work.path}";

  hostName = moduleArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");

  leptonSrc = firefox-lepton-ui.src;
  leptonSettings = import ./lepton-settings.nix;
  # These CSS files must be `@import`ed in order to preserve relative URIs
  # expected for icons.
  leptonChrome = ''
    @import url("${leptonSrc}/css/leptonChrome.css");
  '';
  leptonContent = ''
    @import url("${leptonSrc}/css/leptonContent.css");
  '';

  makeProfileSettings = profile:
    writeText "user.js" (concatStrings
      (mapAttrsToList (name: value: ''
          user_pref("${name}", ${builtins.toJSON value});
        '')
        cfg.profiles.${profile}.settings));

  makeProfileSettingsFile = profile:
    runCommand "firefox-${profile}-settings" {} ''
      cat '${leptonSrc}/user.js' '${makeProfileSettings profile}' > $out
    '';

  userChrome = leptonChrome;
  userContentVars = ''
    :root {
      --dotfield-font-family-serif: "${fonts.serif.family}", serif;
      --dotfield-font-family-sans-serif: "${fonts.sans.family}", sans-serif;
      --dotfield-font-family-mono: "${fonts.mono.family}", monospace;
    }
  '';
  userContent =
    userContentVars
    + leptonContent
    + (builtins.readFile ./userContent.css);

  # TODO: consider removing. if you want a firefox without telemetry, use
  # librewolf. otherwise let mozilla do its thing.
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
    # 0 = Accept all cookies by default
    # 1 = Only accept from the originating site (block third-party cookies)
    # 2 = Block all cookies by default
    # 3 = Block cookies from unvisited sites
    # 4 = New Cookie Jar policy (prevent storage access to trackers)
    "network.cookie.cookieBehavior" = 1;

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

      "font.default.x-western" = fonts.sans.family;
      "font.name.monospace.x-western" = fonts.mono.family;
      "font.name.sans-serif.x-western" = fonts.sans.family;
      "font.name.serif.x-western" = fonts.serif.family;
      "font.size.monospace.x-western" = fonts.mono.size;

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
in {
  imports = [
    ./extensions.nix
  ];

  xdg.configFile."tridactyl".source = ./tridactyl;

  home.file = {
    "${homeProfilePath}/chrome/icons".source = leptonSrc + "/icons";
    "${homeProfilePath}/user.js".source = makeProfileSettingsFile "home";

    "${workProfilePath}/chrome/icons".source = leptonSrc + "/icons";
    "${workProfilePath}/user.js".source = makeProfileSettingsFile "work";
  };

  home.packages = optional isLinux (makeDesktopItem {
    name = "firefox-work-profile";
    desktopName = "Firefox (Work)";
    genericName = "Open a Firefox window scoped to the Work profile.";
    icon = "firefox";
    exec = "${cfg.package}/bin/firefox -P ${cfg.profiles.work.path}";
    categories = ["Application" "Network" "WebBrowser"];
  });

  programs.firefox = {
    enable = true;
    package =
      if isDarwin
      then runCommand "firefox-0.0.0" {} "mkdir $out"
      else
        firefox-wayland.override {
          cfg = {
            enableGnomeExtensions = hasGnomeShell;
            enableTridactylNative = true;
          };
        };

    profiles.home = {
      inherit userChrome userContent;
      id = 0;
      settings =
        defaultSettings
        // privacySettings
        // leptonSettings;
    };

    profiles.work = {
      inherit userChrome userContent;
      id = 1;
      settings =
        defaultSettings
        // {
          "browser.startup.homepage" = "about:blank";
          "browser.urlbar.placeholderName" = "Search";
          "privacy.trackingprotection.enabled" = true;
        };
    };
  };
}
##: References
#
# - https://github.com/cmacrae/config/blob/5a32507753339a2ee45155b78b76fda0824002a0/modules/macintosh.nix#L331-L407
# - https://restoreprivacy.com/firefox-privacy/
