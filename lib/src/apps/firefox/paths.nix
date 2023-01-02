# via https://github.com/nix-community/home-manager/blob/e1f1160284198a68ea8c7fffbbb1436f99e46ef9/modules/programs/firefox.nix#L11-L20
hostPlatform: rec {
  mozillaConfig =
    if hostPlatform.isDarwin
    then "Library/Application Support/Mozilla"
    else ".mozilla";
  firefoxConfig =
    if hostPlatform.isDarwin
    then "Library/Application Support/Firefox"
    else "${mozillaConfig}/firefox";
  profiles =
    if hostPlatform.isDarwin
    then "${firefoxConfig}/Profiles"
    else firefoxConfig;
}
