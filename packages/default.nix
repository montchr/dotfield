{
  self,
  lib,
  ...
}: let
  inherit (self) inputs;
  inherit (inputs.flake-utils.lib) filterPackages flattenTree;
  inherit (inputs.gitignore.lib) gitignoreSource;
  l = lib // builtins;

  generatedSources = pkgs: pkgs.callPackage (import ./sources/_sources/generated.nix) {};

  # inspired by the definitions for the eponymous functions in nixpkgs:
  # https://github.com/NixOS/nixpkgs/blob/738fe494da28777ddeb2612c70a5dc909958df4b/pkgs/top-level/splice.nix
  splice = pkgs: let
    sources = generatedSources pkgs;
    mashedPackages = pkgs // (packages pkgs);
    splicePackages = name:
      mashedPackages
      // {
        # TODO: use `cleanSource` from nixpkgs
        inherit gitignoreSource;
        source = sources.${name};
      };
  in {
    inherit splicePackages;
    callPackage = name: l.callPackageWith (splicePackages name);
    callPackages = name: l.callPackagesWith (splicePackages name);
  };

  packageIndex = {
    ##: application helpers
    # firefox-lepton-ui = {source}: source.src;
    kitty-get-window-by-platform-id = ./applications/kitty/get-window-by-platform-id;

    ##: drivers
    epson-201212w = ./drivers/epson_201212w;

    ##: fonts
    sf-pro = ./fonts/sf-pro.nix;
  };

  packages = pkgs: let
    inherit (pkgs) callPackages;
    pkgs' = splice pkgs;
    dotfieldPackages = l.mapAttrs (n: v: pkgs'.callPackage n v {}) packageIndex;
    firefox-addons = l.recurseIntoAttrs (callPackages ./applications/firefox/firefox-addons {});
    # TODO: remove the need for sources outside of this flake module -- package everything beforehand
    sources = generatedSources pkgs;
  in
    dotfieldPackages // {inherit dotfieldPackages firefox-addons sources;};

  makeOverlay = f: (final: _prev: (f (packages final)));
in {
  flake.overlays = {
    packages = makeOverlay (pkgs': pkgs'.dotfieldPackages);
    firefox-addons = makeOverlay (pkgs': {firefox-addons = pkgs'.firefox-addons.addons;});
    # TODO: remove the need for sources outside of this flake module -- package everything beforehand
    sources = makeOverlay (pkgs': {inherit (pkgs') sources;});
  };
  perSystem = {
    pkgs,
    system,
    ...
  }: {
    packages = filterPackages system (flattenTree (packages pkgs));
  };
}
