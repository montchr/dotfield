pkgs: let
  inherit (pkgs) callPackage gitignoreSource sources;
in {
  ##: nvfetcher sources
  sources = callPackage ./sources/_sources/generated.nix {};

  ##: internal packages
  dotfield-config = callPackage ./dotfield/dotfield-config.nix {};

  ##: application helpers
  kitty-set-app-icon = callPackage ./applications/kitty/set-app-icon.nix {};
  kitty-get-window-by-platform-id = callPackage ./applications/kitty/get-window-by-platform-id.nix {};

  ##: development tools
  ediff-tool = callPackage ./development-tools/ediff-tool {};
  git-submodule-rewrite = callPackage ./development-tools/git-submodule-rewrite {};

  ##: drivers
  epson-201212w = callPackage ./drivers/epson_201212w {};

  ##: fonts
  nerdfonts-symbols-only = callPackage ./fonts/nerdfonts-symbols-only.nix {};
  # pragmatapro = ./fonts/pragmatapro.nix {};
  sf-pro = callPackage ./fonts/sf-pro.nix {};

  ##: golang packages
  trellis-cli = callPackage ./golang/trellis-cli {inherit sources;};

  ##: php packages
  phpactor = callPackage ./php/phpactor {};

  ##: python packages
  cpanel-cli = callPackage ./python/cpanel-cli {inherit sources;};
  hpi = callPackage ./python/HPI {};
  orgparse = callPackage ./python/orgparse {};
  promnesia = callPackage ./python/promnesia {
    inherit (pkgs) hpi orgparse;
  };
}
