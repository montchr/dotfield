_hmArgs: {
  imports = [
    ./profile.nix

    ../../home/mixins/workstation.nix

    ../../home/profiles/git/with-gpg-signing.nix
  ];

  home.stateVersion = "21.11";
}
