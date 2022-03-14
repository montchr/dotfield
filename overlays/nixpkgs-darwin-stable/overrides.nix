channels: final: prev: {
  inherit (channels.nixpkgs-unstable)
    direnv
    git-cliff
    neovim
    neovim-unwrapped
    nix
    nix-direnv
    ;
}
