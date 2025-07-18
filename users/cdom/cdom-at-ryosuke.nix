_hmArgs: {
  imports = [
    ./profile.nix

    ../../home/mixins/workstation.nix
    ../../home/mixins/jobwork.nix

    ../../home/profiles/git/with-gpg-signing.nix
    ../../home/profiles/gpg/with-ssh-support.nix
  ];

  home.stateVersion = "22.05";
}
