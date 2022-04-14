channels: final: prev: {
  inherit
    (channels.nixpkgs-unstable)
    alejandra
    neovim
    neovim-unwrapped
    nix
    ;
}
