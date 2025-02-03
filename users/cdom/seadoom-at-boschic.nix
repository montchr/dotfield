_hmArgs: {
  imports = [
    ../../home/mixins/workstation.nix

    ../../home/profiles/git/with-pgp-signing.nix
  ];

  home.stateVersion = "21.11";
}
