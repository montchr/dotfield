{inputs, ...}: let
  inherit (inputs.flake-utils.lib) filterPackages flattenTree;
  l = inputs.nixpkgs.lib // builtins;
in {
  perSystem = ctx @ {
    pkgs,
    system,
    ...
  }: let
    sources = pkgs.callPackage (import ./sources/_sources/generated.nix) {};
    firefox-addons = l.recurseIntoAttrs (pkgs.callPackages ./applications/firefox/firefox-addons {});
  in {
    _module.args.packages = ctx.config.packages;
    _module.args.sources = sources // {inherit firefox-addons;};
    packages = filterPackages system (flattenTree {
      ##: application helpers
      kitty-get-window-by-platform-id = pkgs.callPackage ./applications/kitty/get-window-by-platform-id {};

      ##: drivers
      epson-201212w = pkgs.callPackage ./drivers/epson_201212w {};

      ##: fonts
      sf-pro = pkgs.callPackage ./fonts/sf-pro.nix {};
    });
  };
}
