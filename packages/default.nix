{
  perSystem = {pkgs, ...}: {
    packages = {
      berkeley-mono = pkgs.callPackage ./fonts/berkeley-mono.nix {};
      cod = pkgs.callPackage ./tools/cod/package.nix {};
      epson-201212w = pkgs.callPackage ./drivers/epson_201212w {};
      ddi = pkgs.callPackage ./tools/ddi.nix {};
      firefox-ui-fix = pkgs.callPackage ./applications/firefox/firefox-ui-fix/package.nix {};
      igr = pkgs.callPackage ./tools/igr.nix {};
      kitty-get-window-by-platform-id = pkgs.callPackage ./applications/kitty/get-window-by-platform-id {};
      sf-pro = pkgs.callPackage ./fonts/sf-pro.nix {};
    };
  };
}
