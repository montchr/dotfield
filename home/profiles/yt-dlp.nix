{ pkgs, flake, ... }:
let
  inherit (flake.inputs.apparat.lib.yt-dl) maxRatio;
in
{
  programs.yt-dlp = {
    enable = true;
    package = pkgs.yt-dlp.override {
      # Provide back-compat for `youtube-dl`
      withAlias = true;
    };
    settings = {
      embed-thumbnail = true;
      embed-subs = true;
      # TODO: confirm value
      # sub-langs = "en-US";
    };
  };
  home.shellAliases = {
    ytd = "yt-dlp";

    # Video download should prefer 480p and max download rate is 2 MiB/s.
    ytd480 = ''youtube-dl -f "${maxRatio 480}" -r2m'';

    # Video download that prefers 720p and max download rate is 2 MiB/s.
    ytd720 = ''youtube-dl -f "${maxRatio 720}" -r2m'';

    # Video download that prefers 1080p and max download rate is 5 MiB/s.
    ytd1080 = ''youtube-dl -f "${maxRatio 1080}" -r5m'';
  };
}
###: Sources:
# - <https://git.sr.ht/~rycee/configurations/tree/fc2cf950844a23d09580b86e45d16a9dc39d9c5e/item/user/common.nix#L11-14>
# - <https://git.sr.ht/~rycee/configurations/tree/fc2cf950844a23d09580b86e45d16a9dc39d9c5e/item/user/common.nix#L538-549>
