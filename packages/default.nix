{
  self,
  lib,
  ...
}: let
  inherit (self) inputs;
  inherit (inputs.flake-utils.lib) filterPackages flattenTree;
  inherit (inputs.gitignore.lib) gitignoreSource;
  inherit
    (builtins)
    baseNameOf
    functionArgs
    intersectAttrs
    isPath
    mapAttrs
    toString
    ;
  inherit
    (lib)
    callPackageWith
    callPackagesWith
    recurseIntoAttrs
    removeSuffix
    ;

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
    callPackage = name: callPackageWith (splicePackages name);
    callPackages = name: callPackagesWith (splicePackages name);
  };

  packageIndex = {
    ##: internal packages
    dotfield-config = ./dotfield/dotfield-config.nix;
    repl = ./dotfield/fup-repl;

    ##: application helpers
    # firefox-lepton-ui = {source}: source.src;
    # FIXME: the magical "withDeps" approach won't work here because it requires a source of a different name
    # kitty-set-app-icon = ./applications/kitty/set-app-icon;
    kitty-get-window-by-platform-id = ./applications/kitty/get-window-by-platform-id;

    ##: development tools
    ediff-tool = ./development-tools/ediff-tool;
    git-submodule-rewrite = ./development-tools/git-submodule-rewrite;

    ##: drivers
    epson-201212w = ./drivers/epson_201212w;

    ##: fonts
    nerdfonts-symbols-only = ./fonts/nerdfonts-symbols-only.nix;
    # pragmatapro = ./fonts/pragmatapro.nix;
    sf-pro = ./fonts/sf-pro.nix;

    ##: golang packages
    trellis-cli = ./golang/trellis-cli;

    ##: php packages
    phpactor = ./php/phpactor;

    ##: python packages
    cpanel-cli = ./python/cpanel-cli;

    # FIXME: disabled due to pyopenssl dependency
    # hpi = ./python/HPI;
    # orgparse = ./python/orgparse;
    # promnesia = ./python/promnesia;
  };

  packages = pkgs: let
    inherit (pkgs) callPackages;
    pkgs' = splice pkgs;
    dotfieldPackages = mapAttrs (n: v: pkgs'.callPackage n v {}) packageIndex;
    iosevka-xtal = recurseIntoAttrs (callPackages ./fonts/iosevka-xtal.nix {});
    firefox-addons = recurseIntoAttrs (callPackages ./applications/firefox/firefox-addons {});
  in
    dotfieldPackages // {inherit dotfieldPackages iosevka-xtal firefox-addons;};

  makeOverlay = f: (final: prev: (f (packages final)));
in {
  flake.overlays = {
    packages = makeOverlay (pkgs': pkgs'.dotfieldPackages);
    iosevka = makeOverlay (pkgs': {inherit (pkgs') iosevka-xtal;});
    firefox-addons = makeOverlay (pkgs': {firefox-addons = pkgs'.firefox-addons.addons;});
  };
  perSystem = ctx @ {
    pkgs,
    system,
    config,
    inputs',
    ...
  }: let
    packagesArg = {
      inherit (inputs'.agenix.packages) agenix;
      inherit
        (inputs'.gitignore.packages)
        gitignoreSource
        gitignoreSourceWith
        gitignoreFilter
        gitignoreFilterWith
        ;
    };
    sources = generatedSources pkgs;
  in {
    # TODO: remove the need for sources outside of this flake module -- package everything beforehand
    _module.args.sources = sources;
    packages = filterPackages system (flattenTree (packages pkgs));
  };
}
