channels: final: prev: {
  inherit
    (channels.nixpkgs-unstable)
    alejandra
    awscli2
    direnv
    git-cliff
    neovim
    neovim-unwrapped
    nix
    nix-direnv
    ;
}
