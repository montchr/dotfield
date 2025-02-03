_hmArgs: {
  imports = [
    ../../home/mixins/workstation.nix

    ../../home/profiles/git/with-pgp-signing.nix
    ../../home/profiles/shells/fish/default.nix
    ../../home/profiles/graphical/applications/qutebrowser.nix
  ];

  home.stateVersion = "21.11";
}
