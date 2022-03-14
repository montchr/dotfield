channels: final: prev: {
  inherit (channels.nixpkgs-unstable)
    direnv
    neovim
    neovim-unwrapped
    nix
    nix-direnv
    ;
  inherit (channels.nixpkgs-trunk)
    # TODO: inherit from unstable when v0.6.0 is available there
    git-cliff
    ;
}
