{
  nix.settings = {
    trusted-substituters = [ "ssh://eu.nixbuild.net" ];
    trusted-public-keys = [ "nixbuild.net/cdom-1:DU7hcG2k5kj9nC6NUvsOYQNiaI5UXYjjY5gBOccaND4=" ];
  };
}
