{
  imports = [
    ./developer.nix
    ./graphical.nix
    ./media-manager.nix
    ./trusted.nix

    ../profiles/graphical/applications/kitty/default.nix
    ../profiles/graphical/applications/obs-studio.nix
    ../profiles/graphical/applications/okular.nix
    ../profiles/graphical/applications/xournal.nix
    ../profiles/graphical/espanso.nix
    ../profiles/development/common.nix
    ../profiles/development/data-wrangling.nix
    ../profiles/emacs/org-protocol.nix
    ../profiles/git/repo-manager.nix
    ../profiles/git/with-pgp-signing.nix
    ../profiles/git-sync.nix
    ../profiles/ledger.nix
    ../profiles/pandoc.nix
    ../profiles/sync.nix
    ../profiles/writing.nix
    ../profiles/yubikey.nix
  ];
}
