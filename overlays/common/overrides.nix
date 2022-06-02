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

  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/networking/browsers/firefox/wrapper.nix
  firefox-wayland = (prev.firefox-wayland.override {
    cfg = {
      # forceWayland = true;
      # Gnome shell native connector
      enableGnomeExtensions = true;
      # Tridactyl native connector
      enableTridactylNative = true;
      # Buku bookmarking tool native connector
      enableBukubrow = true;
    };
  });

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};
}
