channels: final: prev: {
  inherit
    (channels.nixpkgs-unstable)
    alejandra
    direnv
    git-cliff
    neovim
    neovim-unwrapped
    nix
    nix-direnv
    ;
}
