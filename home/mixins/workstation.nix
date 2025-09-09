{ flake, pkgs, ... }:
{
  imports = [
    # flake.config.aspects.graphical.home

    ./developer.nix
    ./media-manager.nix
    ./trusted.nix

    ../profiles/graphical/download-manager.nix
    ../profiles/graphical/applications/graphics-editing.nix
    ../profiles/graphical/applications/qutebrowser.nix
    ../profiles/graphical/reading.nix

    ../profiles/development/common.nix
    ../profiles/development/data-wrangling.nix
    # ../profiles/emacs/org-protocol.nix
    ../profiles/espanso/default.nix
    ../profiles/git-sync.nix
    ../profiles/kdeconnect.nix
    ../profiles/ledger.nix
    ../profiles/pandoc.nix
    ../profiles/syncthing.nix
    ../profiles/yubikey.nix
  ];

  home.packages = [
    pkgs.libreoffice-fresh
  ];

  programs.obs-studio.enable = true;
}
