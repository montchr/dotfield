hmArgs @ {
  config,
  inputs,
  pkgs,
  ...
}: let
  inherit (inputs) firefox-lepton-ui;
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

  commonSettings = import ./settings/common.nix hmArgs;
  leptonSettings = import ./settings/lepton.nix;
in {
  programs.firefox.profiles.home = {
    id = 0;
    settings = l.recursiveUpdate commonSettings leptonSettings;
  };

  programs.firefox.profiles.work = {
    id = 1;
    settings = l.recursiveUpdate commonSettings {
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
    mkPrefLine = k: v: "user_pref(\"${k}\", ${l.toJSON v});\n";

    profileUserPrefsJs = l.concatStrings (l.mapAttrsToList mkPrefLine profile.settings);
    profileUserContentVars = ''
      :root {
        --dotfield-font-family-serif: "${fonts.serif.family}", serif;
        --dotfield-font-family-sans-serif: "${fonts.sans.family}", sans-serif;
        --dotfield-font-family-mono: "${fonts.mono.family}", monospace;
      }
    '';
  in {
    "${profileDir}/user.js".source = pkgs.concatText (pnamePrefix "user-js-text") [
      (firefox-lepton-ui + "/user.js")
      (pkgs.writeText (pnamePrefix "user-prefs-js") profileUserPrefsJs)
    ];

    "${profileDir}/chrome/icons".source = firefox-lepton-ui + "/icons";

    "${profileDir}/chrome/userChrome.css".source = pkgs.concatText (pnamePrefix "userChrome-text") [
      (firefox-lepton-ui + "/css/leptonChrome.css")
    ];

    "${profileDir}/chrome/userContent.css".source = pkgs.concatText (pnamePrefix "userContent-text") [
      (firefox-lepton-ui + "/css/leptonContent.css")
      (pkgs.writeText (pnamePrefix "userContent-vars") profileUserContentVars)
      ./userContent.css
    ];
  }));
}
