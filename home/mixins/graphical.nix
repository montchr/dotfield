{
  imports = [
    ../profiles/graphical/applications/chromium.nix
    ../profiles/graphical/applications/firefox/default.nix
    ../profiles/graphical/applications/foot.nix
    ../profiles/graphical/applications/ghostty/default.nix
    #    ../profiles/graphical/applications/kitty/default.nix
    ../profiles/graphical/common.nix

    ../profiles/hardware/keyboard/default.nix
    ../profiles/multimedia/default.nix
    ../profiles/theme/default.nix

    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # vscode
  ];
}
