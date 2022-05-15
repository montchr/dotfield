channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    alejandra
    atuin
    awscli2
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

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};
}
