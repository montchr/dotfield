{inputs, ...}: {
  perSystem = {
    pkgs,
    system,
    ...
  }: {
    packages = inputs.flake-utils.lib.filterPackages system {
      ast-grep = pkgs.callPackage ./development/tools/misc/ast-grep {};
      berkeley-mono = pkgs.callPackage ./data/fonts/berkeley-mono/default.nix {};
      cod = pkgs.callPackage ./shells/cod {};
      epson-201212w = pkgs.callPackage ./misc/drivers/epson_201212w {};
      ddi = pkgs.callPackage ./tools/system/dd/ddi.nix {};
      firefox-ui-fix = pkgs.callPackage ./data/themes/firefox-ui-fix {};
      igr = pkgs.callPackage ./tools/text/igr {};
      kitty-get-window-by-platform-id =
        pkgs.callPackage
        ./applications/terminal-emulators/kitty/get-window-by-platform-id.nix {};
      sf-pro = pkgs.callPackage ./data/fonts/sf-pro {};
    };
  };
}
