channels: final: prev: {
  inherit (channels.nixpkgs-unstable)
    direnv
    neovim
    neovim-unwrapped
    nix
    nix_2_5
    nix_2_6
    nixUnstable
    nix-direnv
    ;
  inherit (channels.nixpkgs-trunk)
    # TODO: inherit from unstable when v0.6.0 is available there
    git-cliff
    ;
}
