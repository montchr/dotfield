_hmArgs: {
  imports = [
    ../../home/mixins/workstation.nix
    ../../home/mixins/jobwork.nix

    ../../home/profiles/shells/fish/default.nix
    ../../home/profiles/shells/fish/trampoline.nix

    ../../home/profiles/git/with-gpg-signing.nix
    ../../home/profiles/gpg/with-ssh-support.nix
  ];

  home.stateVersion = "22.05";
}
