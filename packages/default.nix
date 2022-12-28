{inputs, ...}: let
  inherit (inputs.flake-utils.lib) filterPackages flattenTree;
in {
  perSystem = ctx @ {
    pkgs,
    system,
    ...
  }: {
    _module.args.packages = ctx.config.packages;
    packages = filterPackages system (flattenTree {
      epson-201212w = pkgs.callPackage ./drivers/epson_201212w {};
      ddi = pkgs.callPackage ./tools/ddi.nix {};
      igr = pkgs.callPackage ./tools/igr.nix {};
      fre = pkgs.callPackage ./tools/misc/fre {};
      kitty-get-window-by-platform-id = pkgs.callPackage ./applications/kitty/get-window-by-platform-id {};
      sf-pro = pkgs.callPackage ./fonts/sf-pro.nix {};
    });
  };
}
