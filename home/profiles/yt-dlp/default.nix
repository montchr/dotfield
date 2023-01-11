{pkgs, ...}: {
  imports = [./aliases.nix];
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
}
