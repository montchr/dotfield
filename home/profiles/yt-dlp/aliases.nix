###: Sources:
# - <https://git.sr.ht/~rycee/configurations/tree/fc2cf950844a23d09580b86e45d16a9dc39d9c5e/item/user/common.nix#L11-14>
# - <https://git.sr.ht/~rycee/configurations/tree/fc2cf950844a23d09580b86e45d16a9dc39d9c5e/item/user/common.nix#L538-549>
{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;

  ytdFormat = h: "bestvideo[height<=?${l.toString h}][fps<=?30]+bestaudio/best[height<=?${
    l.toString h
  }][fps<=?30]";
in {
  home.shellAliases = rec {
    # 480p is a fine default.
    ytd = ytd480;

    # Video download should prefer 480p and max download rate is 2 MiB/s.
    ytd480 = ''youtube-dl -f "${ytdFormat 480}" -r2m'';

    # Video download that prefers 720p and max download rate is 2 MiB/s.
    ytd720 = ''youtube-dl -f "${ytdFormat 720}" -r2m'';

    # Video download that prefers 1080p and max download rate is 5 MiB/s.
    ytd1080 = ''youtube-dl -f "${ytdFormat 1080}" -r5m'';
  };
}
