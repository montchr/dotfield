{
  imports = [
    ../profiles/graphical/applications/chromium.nix
    ../profiles/graphical/applications/firefox/default.nix
    ../profiles/graphical/applications/foot.nix
    ../profiles/graphical/applications/kitty/default.nix
    ../profiles/graphical/common.nix
    ../profiles/graphical/media-client.nix

    ../profiles/hardware/keyboard/default.nix
    ../profiles/spotify.nix
    ../profiles/theme/default.nix
    ../profiles/yt-dlp.nix

    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # vscode
  ];
}
