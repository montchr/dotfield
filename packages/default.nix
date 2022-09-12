{self, ...}: let
  inherit (self) inputs;
  inherit (inputs.flake-utils.lib) filterPackages;
  inherit (inputs.gitignore.lib) gitignoreSource;
  inherit
    (builtins)
    functionArgs
    intersectAttrs
    isPath
    mapAttrs
    ;

  packageIndex = {
    ##: internal packages
    dotfield-config = ./dotfield/dotfield-config.nix;

    ##: application helpers
    firefox-lepton-ui = {source}: source.src;
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
    hpi = ./python/HPI;
    orgparse = ./python/orgparse;
    promnesia = ./python/promnesia;
  };

  # packageDeps = name: package: (intersectAttrs
  #   (functionArgs package)
  #   (config.packages
  #     // {
  #       # FIXME: `inputs'` tends to destroy non-system-spaced attrs, this may not work
  #       inherit (inputs'.gitignore.lib) gitignoreSource;
  #       source = sources.${name};
  #     }));

  generatedSources = pkgs:
    pkgs.callPackage
    (import ./sources/_sources/generated.nix) {};
  # {inherit (pkgs) fetchurl fetchzip fetchFromGitHub;};

  dotfieldPackages = pkgs: let
    sources = generatedSources pkgs;
  in
    mapAttrs (name: v: let
      package =
        if (isPath v)
        then (import v)
        else v;
      extraDeps = {
        inherit gitignoreSource;
        source = sources.${name};
      };
    in (pkgs.callPackage package
      (intersectAttrs (functionArgs package) extraDeps)))
    packageIndex;
in {
  flake.overlays = {
    packages = final: prev: (dotfieldPackages final);
  };
  perSystem = ctx @ {
    pkgs,
    system,
    config,
    inputs',
    ...
  }: let
    sources = generatedSources pkgs;
  in {
    # TODO: remove the need for sources outside of this flake module -- package everything beforehand
    _module.args.sources = sources;
    packages = filterPackages system (dotfieldPackages pkgs);
  };
}
