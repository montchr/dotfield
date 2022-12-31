hmArgs @ {
  config,
  inputs,
  pkgs,
  packages,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
  inherit (config.theme) fonts;
  l = inputs.nixpkgs.lib // builtins;

  cfg = config.programs.firefox;

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

  settingsType = with l.types;
    lazyAttrsOf (oneOf [bool int str]);

  evalSettings = modules:
    l.evalModules {
      modules =
        modules
        ++ (l.singleton {
          _module.args.osConfig = hmArgs.osConfig or null;
          _module.args.theme = config.theme;
          _module.freeformType = settingsType;
        });
      specialArgs = {inherit inputs;};
    };

  makeSettings = {modules ? []}:
    evalSettings ([
        ./settings/common.nix
        # ./settings/lepton.nix
      ]
      ++ modules);

  makeSettings' = module: (makeSettings {modules = [module];}).config;
in {
  programs.firefox.profiles.home = {
    id = 0;
    settings = makeSettings' {
      imports = [./settings/dev.nix];
      "browser.startup.homepage" = "https://lobste.rs";
    };
  };

  programs.firefox.profiles.work = {
    id = 1;
    settings = makeSettings' {
      "browser.startup.homepage" = "about:blank";
    };
  };

  home.packages = l.optional isLinux (pkgs.makeDesktopItem {
    name = "firefox-work-profile";
    desktopName = "Firefox (Work)";
    genericName = "Open a Firefox window scoped to the Work profile.";
    icon = "firefox";
    exec = "${cfg.package}/bin/firefox -P ${cfg.profiles.work.path}";
    categories = ["Application" "Network" "WebBrowser"];
  });

  home.file = l.mkMerge (l.flip l.mapAttrsToList cfg.profiles (_: profile: let
    profileDir = "${profilesPath}/${profile.path}";
    pnamePrefix = s: "firefox-profile-${profile.name}-" + s;
    toUserPrefLine = k: v: "user_pref(\"${k}\", ${l.toJSON v});\n";
    leptonCssDir = "${packages.firefox-ui-fix}/chrome/css";
  in {
    "${profileDir}/user.js".source = pkgs.concatText (pnamePrefix "user-js-text") [
      # (packages.firefox-ui-fix + "/user.js")
      (pkgs.writeText (pnamePrefix "user-prefs-js")
        (l.concatStrings (l.mapAttrsToList toUserPrefLine profile.settings)))
    ];

    "${profileDir}/chrome/content-overrides.css".source = ./userContent.css;

    "${profileDir}/chrome/userChrome.css".text = ''
      /* @import url("${leptonCssDir}/leptonChrome.css"); */
    '';

    "${profileDir}/chrome/userContent.css".text = ''
      /*
      NOTE: The import order here is important.
            All imports must occur before `@namespace` declarations,
            which happens at the beginning of `leptonContent.css`.
            This is not a restriction imposed by Lepton or even Mozilla,
            it is a restriction of the CSS specification:
            - <https://github.com/black7375/Firefox-UI-Fix/blob/master/docs/Restrictions.md#import>
            - <https://developer.mozilla.org/en-US/docs/Web/CSS/@namespace>
      */
      @import url("content-overrides.css");
      /* @import url("${leptonCssDir}/leptonContent.css"); */

      :root {
        --dotfield-font-family-serif: "${fonts.serif.family}", serif;
        --dotfield-font-family-sans-serif: "${fonts.sans.family}", sans-serif;
        --dotfield-font-family-mono: "${fonts.mono.family}", monospace;

        --tridactyl-font-family: var(--dotfield-font-family-mono) !important;
        --tridactyl-cmdl-font-family: var(--tridactyl-font-family) !important;
        --tridactyl-status-font-family: var(--tridactyl-font-family) !important;
        --tridactyl-cmplt-font-family: var(--tridactyl-font-family) !important;
        --tridactyl-hintspan-font-family: var(--tridactyl-font-family) !important;
      }
    '';
  }));
}
