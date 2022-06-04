channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    awscli2
    xplr
    ;

  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/networking/browsers/firefox/wrapper.nix
  firefox-wayland = prev.firefox-wayland.override {
    cfg = {
      # forceWayland = true;
      # Gnome shell native connector
      enableGnomeExtensions = true;
      # Tridactyl native connector
      enableTridactylNative = true;
      # Buku bookmarking tool native connector
      enableBukubrow = true;
    };
  };

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};
}
