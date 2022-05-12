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
    zsh-prezto
    ;
}
