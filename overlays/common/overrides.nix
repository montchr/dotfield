channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    alejandra
    atuin
    awscli2
    dconf2nix
    direnv
    git-cliff
    navi
    neovim
    neovim-unwrapped
    nix
    nix-direnv
    rage
    tidy-viewer
    treefmt
    zsh-prezto
    ;
    
  # TODO: remove once available on another channel
  inherit (channels.nixpkgs-trunk)
    visidata;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};
}
